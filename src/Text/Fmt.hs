{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE ViewPatterns         #-}

{- | Format Text or Strings, in a type-safe way, using Quasi Quotations.

    @
    import Text.Fmt   ( fmt )
    import Data.Text  ( pack )

    -- fmt produces Text or String per type inference
    [fmt|This is some %t|] (pack "text") == "This is some text"
    @

    A module to provide type-safe printf-alike functionality in Haskell.  __Note
    that both the QuasiQuotes and OverloadedStrings extensions are necessary in
    practice__, frankly I'm not sure why OverloadedStrings is.  Without it, you
    may get errors a bit like this:

    @
      • Couldn't match expected type ‘Format (Integer → Text) (Int → t)’
                  with actual type ‘[Char]’
      • In the first argument of ‘(%)’, namely
    @
 -}

module Text.Fmt
  ( -- * Format Specifiers

    -- $formatting

    ByteFmtBase(..), FormatTarget(..), ToUTCTimeY( toUTCTimeY )
  , fmt, fmtS, fmtL, fmtT, formatBytes, formatUTCY, formatUTCYDoW
  -- for testing only
  , Token(..), conversion, fill, sprintf, tokens )
where

import Prelude ( Double, Int, Integer, Integral, Real, RealFloat
               , (+), (-), (/), (^), (**)
               , abs, decodeFloat, error, floor, fromIntegral, toInteger
               )

-- base --------------------------------

import Control.Applicative  ( many )
import Data.Bool            ( otherwise )
import Data.Char            ( Char, toUpper )
import Data.Either          ( Either( Left, Right ) )
import Data.Eq              ( Eq )
import Data.Foldable        ( Foldable, foldr, toList )
import Data.Function        ( ($), const, id )
import Data.Functor         ( fmap )
import Data.List            ( concat, elem, intercalate )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Ord             ( (<), (>) )
import Data.Word            ( Word8 )
import GHC.Stack            ( SrcLoc
                            , getCallStack, srcLocFile, srcLocModule
                            , srcLocPackage, srcLocEndCol, srcLocEndLine
                            , srcLocStartCol, srcLocStartLine
                            )
import Numeric              ( logBase )
import Numeric.Natural      ( Natural )
import Text.Read            ( read )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- formatting --------------------------

import qualified  Formatting.Formatters  as  Formatters

import Formatting             ( Format, (%), (%.)
                              , format, formatToString, later, mapf, sformat )
import Formatting.Formatters  ( bin, fixed, hex, int, oct, shortest, shown
                              , stext, text )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens  ( view )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤), (⋪), (⋫), (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Maybe        ( 𝕄 )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.String       ( 𝕊 )
import Data.MoreUnicode.Text         ( 𝕋 )

-- number ------------------------------

import Number  ( ToNum( toNumI ) )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit, noneOf, oneOf, string )
import Text.Parsec.Combinator  ( eof, many1, option, optionMaybe )
import Text.Parsec.Error       ( ParseError )
import Text.Parsec.Prim        ( (<?>), parse, try )

-- parsec-plus-base --------------------

import ParsecPlusBase  ( Parser, boundedDoubledChars )

-- process -----------------------------

import System.Process.Internals  ( translate )

-- template-haskell --------------------

import Language.Haskell.TH  ( ExpQ, Name, appE, charL, conE, infixE, integerL
                            , litE, stringL, varE )
import Language.Haskell.TH.Quote
                            ( QuasiQuoter( QuasiQuoter, quoteDec
                                         , quoteExp, quotePat, quoteType ) )

-- text --------------------------------

import qualified  Data.Text               as  Text
import qualified  Data.Text.Lazy          as  LazyText
import qualified  Data.Text.Lazy.Builder  as  LazyBuilder

import Data.Text  ( dropWhileEnd, pack, unpack )

-- text-format -------------------------

import Data.Text.Buildable  as  Buildable

-- time --------------------------------

import Data.Time.Clock   ( UTCTime )
import Data.Time.Format  ( defaultTimeLocale, formatTime )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Text.Fmt.Token  ( Token( Conversion, Str ) )

-------------------------------------------------------------------------------

-- | tokenize a string into strings & conversions
tokens ∷ 𝕋 → Either ParseError [Token]
tokens s = concatTokens ⊳ parse (tokenP ⋪ eof) (unpack s) s

----------------------------------------

-- | squish consecutive Str together

concatTokens ∷ [Token] → [Token]
concatTokens (Str s : Str s' : ts) = concatTokens (Str (s ⊕ s') : ts)
concatTokens (t : t' : ts)         = t : t' : ts
concatTokens ts                    = ts

----------------------------------------

-- | parse a string into tokens
tokenP ∷ Parser [Token]
tokenP = many (simpleStr ∤ try escapePC ∤ try escapeSlash ∤ conversion)

----------------------------------------

{- | Parse a string into a conversion specifier. -}
conversion ∷ Parser Token
conversion =
  Conversion ⊳ (string "%" ⋫ optionMaybe fill)
             ⊵ optionMaybe precision
             ⊵ optionMaybe (pack ⊳ boundedDoubledChars '{' '}')
             ⊵ (oneOf "bdefIkKlLnoqQstTwxyYzZ" <?> "valid conversion char")

----------------------------------------

{- | Parser for the fill spec of a conversion (the -07 of "%-07.4s", for
     example). -}
fill ∷ Parser (Integer, Char)
fill = (\ a b c d → (read (concat [a,[c],d]), b)) ⊳ option "" (string "-")
                                                  ⊵ option ' ' (char '0')
                                                  ⊵ oneOf "123456789"
                                                  ⊵ many digit

----------------------------------------

-- | parse for the precision part of a conversion (.2 of "%3.2f", for example)

precision ∷ Parser Natural
precision = read ⊳ (char '.' ⋫ many digit)

----------------------------------------

-- | parser for an unadorned string (without any % chars)
simpleStr ∷ Parser Token
simpleStr = Str ⊳ many1 (noneOf "%\\")

----------------------------------------

-- | parser for an escaped '%' (represented in the incoming string as "%%")
escapePC ∷ Parser Token
escapePC = Str ⊳ const "%" ⊳ string "%%"

----------------------------------------

-- | parser for slash escapes, e.g., \\, \n, \t
escapeSlash ∷ Parser Token
escapeSlash = Str ∘ decode ⊳ (char '\\' ⋫ oneOf "nt\\")
              where decode 'n'  = "\n"
                    decode 't'  = "\t"
                    decode '\\' = "\\"
                    decode c    = error $ ю [ "bad decode char: '", [c], "'" ]

----------------------------------------

{- | whether to format a bytes value in terms of powers of 10^3, or 2^10 -}
data ByteFmtBase = B_1000 | B_1024
  deriving Eq

-- | try really hard to fit within 7 chars
formatBytes ∷ (Formatters.Buildable a, Integral a) ⇒ ByteFmtBase → a → 𝕋
formatBytes _ (toInteger → 0) = "0"
formatBytes b bs =
    case b of
      B_1000 → go 1000 bs -- (byteSize bs)
      B_1024 → go 1024 bs -- (fromIntegral $ byteSize bs)
    where go ∷ (Formatters.Buildable b, Integral b) ⇒ Double → b → 𝕋
          go x bytes =
            let ex ∷ Word8 = floor (logBase x $ fromIntegral bytes)
                (pfx,exp) ∷ (𝕄 Char, Word8)= case ex of
                              0 → (Nothing,  0)
                              1 → (Just 'k', 1)
                              2 → (Just 'M', 2)
                              3 → (Just 'G', 3)
                              4 → (Just 'T', 4)
                              5 → (Just 'P', 5)
                              6 → (Just 'E', 6)
                              7 → (Just 'Z', 7)
                              _ → (Just 'Y', 8)
                formatB n = fixed n % Formatters.char % Formatters.string % "B"
                i = if b ≡ B_1024 then "i" else ""
             in case pfx of
                 Nothing → sformat (int % "B") bytes
                 Just c  → let mant = fromIntegral bytes / (x^exp)
                               c_   = if b ≡ B_1024 then toUpper c else c
                             in if mant < 10
                                then -- [fmt|%3.2f%T%sB|]
                                     sformat (formatB 2) mant c_ i
                                else if mant < 100
                                     then -- [fmt|%4.1f%T%sB|]
                                          sformat (formatB 1) mant (toUpper c) i
                                     else -- [fmt|%4f%T%sB|]
                                          sformat (formatB 0) mant (toUpper c) i

----------------------------------------

class ToUTCTimeY α where
  toUTCTimeY ∷ α → 𝕄 UTCTime

instance ToUTCTimeY UTCTime where
  toUTCTimeY = Just

instance ToUTCTimeY (𝕄 UTCTime) where
  toUTCTimeY = id

{- | Format a (Maybe UTCTime), in almost-ISO8601-without-fractional-seconds
     (always in Zulu). -}
formatUTCY ∷ ToUTCTimeY α ⇒ α → 𝕋
formatUTCY mt = case toUTCTimeY mt of
                  Just t  → pack $ formatTime defaultTimeLocale "%FZ%T" t
                  Nothing → "-------------------"

{- | Format a (Maybe UTCTime), in ISO8601-without-fractional-seconds (always in
     Zulu), with a leading 3-letter day-of-week. -}
formatUTCYDoW ∷ ToUTCTimeY α ⇒ α → 𝕋
formatUTCYDoW mt = case toUTCTimeY mt of
                     Just t  → pack $ formatTime defaultTimeLocale "%FZ%T %a" t
                     Nothing → "-----------------------"

toFormatUTC ∷ ToUTCTimeY α ⇒ Format ρ (α → ρ)
toFormatUTC = later $ LazyBuilder.fromText ∘ formatUTCY

toFormatUTCDoW ∷ ToUTCTimeY α ⇒ Format ρ (α → ρ)
toFormatUTCDoW = later $ LazyBuilder.fromText ∘ formatUTCYDoW

----------------------------------------

renderStackLine ∷ (𝕊,SrcLoc) → 𝕊
renderStackLine (fname,loc) = let to x y = x ⊕ "→" ⊕ y
                                  toS x y = to (show x) (show y)
                                  col l c = l ⊕ "[" ⊕ c ⊕ "]"
                                  colS l c = col (show l) (show c)
                                  pkg = srcLocPackage   loc
                                  mod = srcLocModule    loc
                                  fn  = srcLocFile      loc
                                  sc  = srcLocStartCol  loc
                                  sl  = srcLocStartLine loc
                                  ec  = srcLocEndCol    loc
                                  el  = srcLocEndLine   loc
                                  st  = colS sl sc
                                  ed  = colS el ec
                                  src = ю [ pkg, ":", mod, ":" ⊕ fn ]
                                  lc = if sl ≡ el
                                       then ю [ col (show sl) (sc `toS` ec) ]
                                       else st `to` ed
                               in ю [ "«", fname, "»", " (", src, "#", lc, ")" ]

----------------------------------------

formatStackHead ∷ HasCallstack α ⇒ α → 𝕊
formatStackHead a = case getCallStack (a ⊣ callstack) of
                      []          → "«NO STACK»"
                      (loc:_) → renderStackLine loc

toFormatStackHead ∷ HasCallstack α ⇒ Format ρ (α → ρ)
toFormatStackHead = later $ LazyBuilder.fromString ∘ formatStackHead

----------------------------------------

formatCallStack ∷ HasCallstack α ⇒ α → 𝕊
formatCallStack (getCallStack ∘ view callstack → ss) =
  case ss of
    [] → "«NO STACK»"
    _  → intercalate "\n" $ renderStackLine ⊳ ss

toFormatCallStack ∷ HasCallstack α ⇒ Format ρ (α → ρ)
toFormatCallStack = later $ LazyBuilder.fromString ∘ formatCallStack

----------------------------------------

-- | parse a fmt, return an ExpQ that when spliced, takes arguments to pass
--   to the formatter to provide a textlike thing (see `FormatTarget`)
sprintf ∷ 𝕋 → ExpQ
sprintf = sprintf_ 'output

-- | like `sprintf`, but always produces a String (to reduce scoped type
--   variables)
sprintfS ∷ 𝕋 → ExpQ
sprintfS = sprintf_ 'formatToString

sprintfT ∷ 𝕋 → ExpQ
sprintfT = sprintf_ 'sformat

sprintfL ∷ 𝕋 → ExpQ
sprintfL = sprintf_ 'format

sprintf_ ∷ Name → 𝕋 → ExpQ
sprintf_ fnam t =
  case tokens t of
    Left  e    → error $ show e
    Right toks → appE (varE fnam) $
                      foldr conjoin (litE $ stringL "") (fmap tokOp toks)
                  where conjoin = infixOp '(%)

{- | Implement a token.  Regular strings pass through; conversions ("%…") are
     implemented, and padded as necessary.
     Conversion token as formatter; e.g., %-3t ⇒ (left 3 ' ') %. stext
 -}
tokOp ∷ Token → ExpQ
-- literal string
tokOp (Str s)                         = litE $ stringL s
-- conversion, no padding
tokOp (Conversion Nothing p t c)      = charOp c Nothing p t
-- conversion, with padding
tokOp (Conversion (Just (width,fll)) prec txt convchar) =
  infixOp '(%.) (fillOp (width,fll)) (charOp convchar (Just width) prec txt)

----------------------------------------

-- create a fill expression
fillIt ∷ Name → Integer → Char → ExpQ
fillIt direction width chr =
  appE (appE (varE direction) (litE (integerL width))) (litE $ charL chr)

{- | Transform a `LazyText` transformer to a `Builder`. -}
buildLTTrans ∷ Buildable ρ ⇒
               (LazyText.Text → LazyText.Text) → ρ → LazyBuilder.Builder
buildLTTrans f =
  LazyBuilder.fromLazyText ∘ f ∘ LazyBuilder.toLazyText ∘ Buildable.build

buildLTFormatter ∷ Buildable ρ ⇒
                   (LazyText.Text → LazyText.Text) → Format α (ρ → α)
buildLTFormatter = later ∘ buildLTTrans

{- | Apply a text transformation to each line of a piece of text. -}
eachLine ∷ Buildable ρ ⇒ (LazyText.Text → LazyText.Text) → Format α (ρ → α)
eachLine f =
  -- we split & intercalate rather than lines/unlines, because the latter is
  -- lossy where the last "line" does or does not end in a newline
  buildLTFormatter $ LazyText.intercalate "\n" ∘ fmap f ∘ LazyText.split (≡'\n')

{- | Pad out each line to (to the left) a given width with a given character. -}
lefts ∷ Buildable ρ ⇒ Integer → Char → Format α (ρ → α)
lefts k c = eachLine (LazyText.justifyRight (fromIntegral k) c)

{- | Pad out each line to (to the right) a given width with a given character.-}
rights ∷ Buildable ρ ⇒ Integer → Char → Format α (ρ → α)
rights k c = eachLine (LazyText.justifyLeft (fromIntegral k) c)

-- | conversion fill; -x → left, (+)x → right

fillOp ∷ (Integer,Char) → ExpQ
fillOp (i,c) | i < 0     = fillIt 'rights (abs i) c
             | i > 0     = fillIt 'lefts       i  c
             | otherwise = -- i ≡ 0 : something's gone wrong!
                           error "cannot fill with size 0"
----------------------------------------

toTextF ∷ Printable t ⇒ Format r (t → r)
toTextF = later $ LazyBuilder.fromText ∘ toText

toTextListF ∷ (Foldable f, Printable t) ⇒ Format r (f t → r)
toTextListF =
  later $ LazyBuilder.fromText ∘ Text.intercalate "," ∘ fmap toText ∘ toList

----------------------------------------

toShell ∷ Printable t ⇒ Format r (t → r)
toShell = later $ LazyBuilder.fromString ∘ translate ∘ toString

toShellList ∷ (Foldable f, Printable t) ⇒ Format r (f t → r)
toShellList =
  let quote = toText ∘ translate ∘ toString
   in later $ LazyBuilder.fromText ∘ Text.intercalate " " ∘ fmap quote ∘ toList

----------------------------------------

toFormatBytes ∷ (Formatters.Buildable a, Integral a) ⇒
                ByteFmtBase → Format r (a → r)
toFormatBytes b = later $ LazyBuilder.fromText ∘ formatBytes b

----------------------------------------

{- $formatting

   Each specfier may be preceded by an integer value to specify padding and
   justification.  A positive integer pads to the left (thus, justifies to the
   right; and a negative integer pads to the right (thus, justifies to the
   left).

   >>> [fmtT|[%3s]|] "a"
   "[  a]"

   >>> [fmtT|[%-3s]|] "a"
   "[a  ]"

   Where noted below, some specifiers also allow a precision (after a 'decimal
   point').

   [@L@] - A `Foldable` of things, where the things are instances of
           `Printable`, joined with ',', thus
           @ (`Foldable` φ, Printable τ) ⇒ φ intercalate "," (fmap toText τ) @

   [@l@] - LazyText `LazyText.Text`

   [@s@] - `String`

   [@t@] - StrictText `Text`

   [@T@] - `Printable` @ τ ⇒ toText τ @

   [@w@] - `Show` @ ω ⇒ show ω @

   [@d@] - `Integral` α ⇒ render as denary

   [@n@] - `ToNum` α    ⇒ render as denary

   [@x@] - `Integral` α ⇒ render as hexadenary

   [@b@] - `Integral` α ⇒ render as binary

   [@o@] - `Integral` α ⇒ render as octal

   [@f@] - `Real` α ⇒ Render as decimal with as many decimal places as
                       necessary.  Beware floating-point representation which
                       may give lengthy results.

   [@f.n@] - `Real` α ⇒ Render as decimal with precisely /n/ decimal places.
                        Will round to the nearest decimal place as appropriate.

   [@e@] - `Real` α ⇒ Render as decimal in scientific notation with 0 decimal
                       places in the mantissa.

                       >>> [fmtT|[%-e]|] (3.14 ∷ Float)
                       "[3e0]"

                       Note that the padding width, if provided, applies to the
                       whole representation; thus the below adds one space
                       because "3e-1" is 4 characters.

                       >>> [fmtT|[%5e]|] (0.314 ∷ Float)
                       "[ 3e-1]"


   [@e.n@] - `Real` α ⇒ Render as decimal in scientific notation with precisely
                         /n/ decimal places in the mantissa.

                         >>> [fmtT|[%-.1e]|] (0.314 ∷ Float)
                         "[3.1e-1]"

   [@y@] - `Integral` α ⇒ Render as bytes, with a 2^10 multiplier.  This tries
                           to fit the value into 7 characters.

                           >>> [fmtT|%y|] (1024^(2∷Int) ∷ Integer)
                           "1.05MB"

                           >>> [fmtT|%y|] (1024 ∷ Integer)
                           "1.02kB"

                           >>> [fmtT|%y|] (999 ∷ Integer)
                           "999B"


   [@Y@] - `Integral` α ⇒ Render as bytes, with a 2^10 multiplier.  This tries
                           to fit the value into 7 characters.

                           >>> [fmtT|%Y|] (999 ∷ Integer)
                           "999B"

                           >>> [fmtT|%Y|] (1024*1023 ∷ Integer)
                           "1023KiB"

                           >>> [fmtT|%Y|] (1024*1024 ∷ Integer)
                           "1.00MiB"

   [@z@] - `UTCTime` α or `Maybe UTCTime` α ⇒ Render as UTCTime, in the form
                                              "YYYY-MM-DDZhh:mm:ss".
                           >>> getCurrentTime >>= return . [fmtT|%z|]
                           "2020-04-20Z05:58:47"

   [@Z@] - `UTCTime` α or `Maybe UTCTime` α ⇒ Render as UTCTime, in the form
                                              "YYYY-MM-DDZhh:mm:ss www" where
                                              www is three-letter day-of-week.
                           >>> getCurrentTime >>= return . [fmtT|%Z|]
                           "2020-04-20Z05:58:47 Mon"

   [@k@] - `ToCallStack` α ⇒ Render the top line of a callstack.

   [@K@] - `ToCallStack` α ⇒ Render a callstack *as multiple lines*.  Note
                             that the behaviour of basic numeric fills with
                             multiple lines is undefined; you might want to use
                             a `{…}` clause here to provide indenting.

   [@q@] - `Printable` @ τ ⇒ `translate` t @; shell-quote string.
   [@Q@] - A `Foldable` of things, where the things are instances of
           `Printable`, which are shell-quoted like @q@, and joined with ' '.
-}

{- | Character op: non-Nothing precision causes error. -}
charOpNoPrecision ∷ ExpQ → Char → 𝕄 Natural → 𝕄 𝕋 → ExpQ
charOpNoPrecision f _ Nothing Nothing = f
charOpNoPrecision _ chr (Just prec) Nothing =
  error $ ю [ "conversion char '", [chr], "' does not admit precision ("
            , show prec, ")" ]
charOpNoPrecision _ chr Nothing (Just t) =
  error $ ю [ "conversion char '", [chr], "' admits no text ({", unpack t,"})" ]
charOpNoPrecision _ chr (Just prec) (Just t) =
  error $ ю [ "conversion char '", [chr], "' admits neither precision ("
            , show prec, ")", " nor text ({", unpack t
            , "})" ]

{- | Conversion character as formatter; e.g., 't' → stext; takes fill width &
     precision too, lest that affect the conversion. -}
charOp ∷ Char          -- ^ conversion character (typically for errmsgs)
       → 𝕄 Integer -- ^ fill width
       → 𝕄 Natural -- ^ precision
       → 𝕄 𝕋    -- ^ optional text (between {}) (unused in charOp)
       → ExpQ

-- list (foldable), joined with ','
charOp c@'L' _ p t = charOpNoPrecision (varE 'toTextListF) c p t
-- lazy text
charOp c@'l' _ p t = charOpNoPrecision (varE 'text) c p t
charOp c@'s' _ p t = charOpNoPrecision (varE 'Formatters.string) c p t
charOp c@'t' _ p t = charOpNoPrecision (varE 'stext) c p t
charOp c@'T' _ p t = charOpNoPrecision (varE 'toTextF) c p t
charOp c@'q' _ p t = charOpNoPrecision (varE 'toShell) c p t
charOp c@'w' _ p t = charOpNoPrecision (varE 'shown) c p t

-- list (foldable) of shell-quoted things, joined with ' '
charOp c@'Q' _ p t = charOpNoPrecision (varE 'toShellList) c p t


charOp c@'d' _ p t = charOpNoPrecision (varE 'int) c p t
charOp c@'x' _ p t = charOpNoPrecision (varE 'hex) c p t
charOp c@'b' _ p t = charOpNoPrecision (varE 'bin) c p t
charOp c@'o' _ p t = charOpNoPrecision (varE 'oct) c p t
charOp c@'n' _ p t = charOpNoPrecision (varE 'tonum) c p t

charOp 'f' _ _ (Just t) =
  error $ "conversion char 'f' admits no text ({" ⊕ unpack t ⊕ "})"
charOp 'f' _ Nothing  Nothing = varE 'floatmin
charOp 'f' _ (Just i) Nothing =
  appE (varE 'fixed) (litE (integerL $ fromIntegral i))
charOp 'e' _ Nothing  Nothing = appE (varE 'expt) (litE (integerL 0))
charOp 'e' _ (Just i) Nothing = appE
  (varE 'expt) (litE (integerL $ fromIntegral i))

charOp c@'y' _ p t =
  charOpNoPrecision (appE (varE 'toFormatBytes) (conE 'B_1000)) c p t
charOp c@'Y' _ p t =
  charOpNoPrecision (appE (varE 'toFormatBytes) (conE 'B_1024)) c p t

charOp c@'z' _ p t = charOpNoPrecision (varE 'toFormatUTC) c p t
charOp c@'Z' _ p t = charOpNoPrecision (varE 'toFormatUTCDoW) c p t

charOp c@'k' _ p t = charOpNoPrecision (varE 'toFormatStackHead) c p t
charOp c@'K' _ p t = charOpNoPrecision (varE 'toFormatCallStack) c p t

charOp x _ _ _ = error $ "bad conversion char'" ⊕ [x] ⊕ "'"

floatmin ∷ Real α ⇒ Format r (α → r)
floatmin = let dropper = dropWhileEnd (`elem` (".0" ∷ 𝕊))
            in later $ LazyBuilder.fromText ∘ dropper ∘ sformat shortest

tonum ∷ ToNum α ⇒ Format r (α → r)
tonum = mapf toNumI int

expt ∷  RealFloat α ⇒ Int → Format r (α → r)
expt i = later (\ f →
  let (m,e ∷ Integer) = decompose f
   in LazyBuilder.fromText $ (sformat $ (fixed i % "e" % int)) m e)


-- | decompose a Real value into "engineering" notation; a mantissa between
--   (-10,10) and an exponent, as a power of 10
decompose ∷ (RealFloat α, Integral β) ⇒ α → (Double, β)
decompose val = let (mant2,ex2) = decodeFloat val
                    mant2d ∷ Double = fromIntegral(abs mant2)
                    ex2d   ∷ Double = fromIntegral ex2
                    res    ∷ Double = log10 mant2d + log10 (2**ex2d)
                    ex10             = floor res
                    log10  ∷ Double → Double = logBase 10
                    mant10 ∷ Double = 10**(res - (fromIntegral ex10∷Double))
                 in if mant2 > 0
                    then (mant10,ex10)
                    else (-mant10,ex10)

----------------------------------------

-- | infix a function between two values
infixOp ∷ Name → ExpQ → ExpQ → ExpQ
infixOp op l r = infixE (Just l) (varE op) (Just r)

----------------------------------------

-- | Generate an instance of FormatTarget (e.g., Strict or Lazy Text, or String)
--   from a format and set of values.
--
--   >>> ([fmt|foo %s|] ("baz" ∷ String)) ∷ Text
--   "foo baz"
--
--   >>> :t [fmtS|bar %t|] ("quux" ∷ Text)
--   [fmtS|bar %t|] ("quux" ∷ Text) ∷ [Char]
--
--   >>> [fmtS|bar %t|] ("quux" ∷ Text)
--  "bar quux"

fmt ∷ QuasiQuoter
fmt =  QuasiQuoter { quoteDec  = error "not implemented"
                   , quoteType = error "not implemented"
                   , quotePat  = error "not implemented"
                   , quoteExp  = sprintf ∘ pack
                   }

--------------------

-- | like `fmt`, but produces specifically a String
fmtS ∷ QuasiQuoter
fmtS =  QuasiQuoter { quoteDec  = error "not implemented"
                    , quoteType = error "not implemented"
                    , quotePat  = error "not implemented"
                    , quoteExp  = sprintfS ∘ pack
                    }

--------------------

-- | like `fmt`, but produces specifically a Lazy Text
fmtL ∷ QuasiQuoter
fmtL =  QuasiQuoter { quoteDec  = error "not implemented"
                    , quoteType = error "not implemented"
                    , quotePat  = error "not implemented"
                    , quoteExp  = sprintfL ∘ pack
                    }

--------------------

-- | like `fmt`, but produces specifically a Strict Text
fmtT ∷ QuasiQuoter
fmtT =  QuasiQuoter { quoteDec  = error "not implemented"
                    , quoteType = error "not implemented"
                    , quotePat  = error "not implemented"
                    , quoteExp  = sprintfT ∘ pack
                    }

------------------------------------------------------------

-- | possible target of `fmt` or similar.

class FormatTarget t where
  output ∷ Format t a → a

instance FormatTarget 𝕋 where
  output = sformat

instance FormatTarget LazyText.Text where
  output = format

instance FormatTarget 𝕊 where
  output = formatToString

-- that's all, folks! ---------------------------------------------------------
