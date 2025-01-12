{-# LANGUAGE UnicodeSyntax #-}
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
  ( ByteFmtBase(..)
  , FormatTarget(..)
  , Justify(..)
  , ToUTCTimeY(toUTCTimeY)
  , columnify
  , commify
  , commifyR
  , fmt
  , fmtL
  , fmtS
  , fmtT
  , formatBytes
  , formatUTCY
  , formatUTCYDoW
    -- for testing only
  , Token(..)
  , conversion
  , fill
  , sprintf
  , tokens
  ) where

import Base0T qualified

import Base0T  hiding ( abs, (÷) )
import Prelude ( Double, Integral, Num, Real, RealFloat, decodeFloat, divMod,
                 error, floor, toRational, (*), (**), (/), (^) )

-- base --------------------------------

import Data.Ratio qualified

import Data.Char     ( isDigit, toUpper )
import Data.Foldable ( Foldable )
import Data.List     ( concat, elem, intercalate, repeat, reverse, transpose,
                       zip, zipWith )
import Data.Maybe    ( fromMaybe )
import Data.Ratio    ( Ratio, denominator, numerator )
import GHC.Stack     ( SrcLoc, getCallStack, srcLocEndCol, srcLocEndLine,
                       srcLocFile, srcLocModule, srcLocPackage, srcLocStartCol,
                       srcLocStartLine )
import Numeric       ( logBase )
import Text.Read     ( read )

-- base-unicode-symbols ----------------

import Prelude.Unicode ( (×) )

-- containers --------------------------

import Data.Map.Lazy qualified as Map

-- formatting --------------------------

import Formatting.Formatters qualified as Formatters

import Formatting            ( Format, format, formatToString, later, mapf,
                               sformat, (%), (%.) )
import Formatting.Formatters ( bin, hex, int, oct, shortest, shown, stext,
                               text )

-- has-callstack -----------------------

import HasCallstack ( HasCallstack(callstack) )

-- lens --------------------------------

import Control.Lens.Each   ( each )
import Control.Lens.Fold   ( (^..) )
import Control.Lens.Getter ( view )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative ( (∤), (⊵), (⋪), (⋫) )
import Data.MoreUnicode.Bool        ( pattern 𝕱, pattern 𝕿 )
import Data.MoreUnicode.Char        ( ℂ )
import Data.MoreUnicode.Functor     ( (⊳) )
import Data.MoreUnicode.Lens        ( (⊣), (⊧) )
import Data.MoreUnicode.Maybe       ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monoid      ( ф, ю )
import Data.MoreUnicode.Semigroup   ( (◇) )
import Data.MoreUnicode.String      ( 𝕊 )
import Data.MoreUnicode.Text        ( 𝕋 )

-- natural -----------------------------

import Natural ( NumSign(SignMinus, SignPlus), length, replicate, unNegate,
                 (⊖) )

-- number ------------------------------

import Number ( ToNum(toNumI) )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, digit, noneOf, oneOf,
                                 string )
import Text.Parser.Combinators ( Parsing, between, choice, eof, option,
                                 optional, try, (<?>) )

-- process -----------------------------

import System.Process.Internals ( translate )

-- safe --------------------------------

import Safe ( maximumDef )

-- template-haskell --------------------

import Language.Haskell.TH       ( ExpQ, Name, appE, charL, infixE, litE,
                                   stringL, varE )
import Language.Haskell.TH.Quote ( QuasiQuoter(QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType) )

-- text --------------------------------

import Data.Text              qualified as Text
import Data.Text.Lazy         qualified as LT
import Data.Text.Lazy.Builder qualified as LazyBuilder

import Data.Text ( dropWhileEnd, pack, unpack )

-- text-format -------------------------

import Data.Text.Buildable as Buildable

-- time --------------------------------

import Data.Time.Clock  ( UTCTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )

-- trifecta ----------------------------

import Text.Trifecta.Parser ( parseString )
import Text.Trifecta.Result ( Result(Failure, Success) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Text.Fmt.Token ( Modifier(MOD_COLON, MOD_COMMIFY, MOD_NONE),
                        Token(Conversion, Str) )

-------------------------------------------------------------------------------

-- size, including impl. for Map, returning ℕ
-- ∈, including for Map & Set
-- check Lens, incl. Control.Lens.At

type RatioN = Ratio ℕ

(÷) ∷ ℕ → ℕ → RatioN
(÷) = (Data.Ratio.%)

abs ∷ ℤ → ℕ
abs = fromIntegral ∘ Base0T.abs

toRatioN ∷ Real α ⇒ α → (NumSign, RatioN)
toRatioN (toRational → a) =
  let num = numerator a
      den = denominator a
      sign = if (num < 0) ≢ (den < 0) then SignMinus else SignPlus
  in  (sign, (abs num) ÷ (abs den))

fixed ∷ Real α ⇒ ℕ → Format β (α → β)
fixed n = Formatters.fixed (fromIntegral n)

instance Eq NumSign where
  SignMinus == SignMinus = 𝕿
  SignPlus  == SignPlus  = 𝕿
  _         == _         = 𝕱

------------------------------------------------------------

-- Copied from ParserPlus; to avoid circular import involving
-- non-empty-containers

betweenCs ∷ CharParsing η ⇒ ℂ → ℂ → η α → η α
betweenCs l r = between (char l) (char r)

doubledChar ∷ CharParsing η ⇒ [ℂ] → η ℂ
doubledChar cs = (choice $ (\ c → char c ⋫ char c) ⊳ cs) ∤ noneOf cs

doubledChars ∷ CharParsing η ⇒ [ℂ] → η 𝕊
doubledChars cs = many (try $ doubledChar cs)

boundedDoubledChars ∷ CharParsing η ⇒ ℂ → ℂ → η 𝕊
boundedDoubledChars l r = betweenCs l r (doubledChars [l,r])

------------------------------------------------------------

(⩻) ∷ Parsing η ⇒ η α → 𝕊 → η α
(⩻) = (<?>)

tokens ∷ 𝕋 → Result [Token]
tokens s = concatTokens ⊳ parseString (tokenP ⋪ eof) ф (unpack s)

----------------------------------------

-- | squish consecutive Str together

concatTokens ∷ [Token] → [Token]
concatTokens (Str s : Str s' : ts) = concatTokens (Str (s ⊕ s') : ts)
concatTokens (t : t' : ts)         = t : t' : ts
concatTokens ts                    = ts

----------------------------------------

-- | parse a string into tokens
tokenP ∷ CharParsing η ⇒ η [Token]
tokenP = many (simpleStr ∤ try escapePC ∤ try escapeSlash ∤ conversion)
         where -- | parser for an unadorned string (without any % chars)
               simpleStr ∷ CharParsing η ⇒ η Token
               simpleStr = Str ⊳ some (noneOf "%\\")

               -- | parser for an escaped '%' (represented in the incoming
               --   string as "%%")
               escapePC ∷ CharParsing η ⇒ η Token
               escapePC = Str ⊳ const "%" ⊳ string "%%"

               -- | parser for slash escapes, e.g., \\, \n, \t
               escapeSlash ∷ CharParsing η ⇒ η Token
               escapeSlash = Str ∘ decode ⊳ (char '\\' ⋫ oneOf "nt\\")
                             where decode 'n'  = "\n"
                                   decode 't'  = "\t"
                                   decode '\\' = "\\"
                                   decode c    =
                                     error $ ю [ "bad decode char: '",[c],"'" ]

-- | parse a string into a conversion specifier
conversion ∷ CharParsing η ⇒ η Token
conversion =
  Conversion ⊳ (string "%"
                  ⋫ option MOD_NONE ((char ',' ⋫ pure MOD_COMMIFY) ∤
                                     (char ':' ⋫ pure MOD_COLON)))
             ⊵ optional fill
             ⊵ optional precision
             ⊵ optional (pack ⊳ boundedDoubledChars '{' '}')
             ⊵ (oneOf "bdefIkKlLmnoqQstTwxyYzZ" ⩻ "valid conversion char")

----------------------------------------

{-| Split a RatioN into hours, minutes, seconds, part -}
hmsp ∷ RatioN → (ℕ,ℕ,ℕ,RatioN)
hmsp secs =
  let num = numerator secs
      den = denominator secs

      (hh,m)  ∷ (ℕ,ℕ)  = num `divMod` (den × 3600)
      (mm,s)  ∷ (ℕ,ℕ)  = m `divMod` (den × 60)
      (ss,p)  ∷ (ℕ,ℕ)  = s `divMod` den
  in  (hh,mm,ss, p ÷ den)

fmtTime_ ∷ (Show α, Real α) ⇒ Modifier → 𝕄 ℕ → α → 𝕋
fmtTime_ mod_ prec (toRatioN → (sign,secs)) | sign ≡ SignMinus =
                                              "-" ◇ fmtTime_ mod_ prec secs
                                            | otherwise     =
  let (hh,mm,ss,part) = hmsp secs

      colon ∷ ℂ → 𝕊
      colon c = case (mod_,c) of
                  (MOD_COLON, 's') → ""
                  (MOD_COLON, _  ) → [':']
                  (_        , _  ) → [c]

      {-| a "0" if input < 10 -}
      p0_10 ∷ (Ord α, Num α) ⇒ α → 𝕊
      p0_10 x | x < 10    = "0"
              | otherwise = ""

      {-| show ℕ, then a character - or colon iff `mod_` ≡ MOD_COLON -}
      show_ ∷ Show α ⇒ α → ℂ → 𝕊
      show_ i chr = show i ◇ colon chr

      {-| like `show_`, but prefix with '0' if required to make a 2-digit num -}
      show2 ∷ (Show α, Ord α, Num α) ⇒ α → ℂ → 𝕊
      show2 i c = p0_10 i ◇ show_ i c

      showS ∷ RatioN → 𝕊
      showS ss_frac  =
        formatToString (fixed $ fromMaybe 0 prec) ss_frac ◇ colon 's'

      show2s ∷ RatioN → 𝕊
      show2s ss_frac = p0_10 ss_frac ◇ showS ss_frac

      showHMSp ∷ (ℕ,ℕ,ℕ,RatioN) → 𝕊
      showHMSp (h',m',s',p') | h' > 0 = ю [ show_ h' 'h', show2 m' 'm'
                                          , show2s ((s'÷ 1) + p')]
                             | m' > 0 = ю [ show_ m' 'm', show2s ((s'÷ 1) + p') ]
                             | otherwise = showS ((s'÷ 1) + p')
  in Text.pack (showHMSp (hh,mm,ss,part))

fmtTime ∷ (Show α, Real α) ⇒ Modifier → 𝕄 ℕ → Format r (α → r)
fmtTime mod_ prec = later $ LazyBuilder.fromText ∘ fmtTime_ mod_ prec

----------------------------------------

{- | Parser for the fill spec of a conversion (the -07 of "%-07.4s", for
     example). -}
fill ∷ CharParsing η ⇒ η (ℤ, ℂ)
fill = (\ a b c d → (read (concat [a,[c],d]), b)) ⊳ option "" (string "-")
                                                  ⊵ option ' ' (char '0')
                                                  ⊵ oneOf "123456789"
                                                  ⊵ many digit

----------------------------------------

-- | parse for the precision part of a conversion (.2 of "%3.2f", for example)

precision ∷ CharParsing η ⇒ η ℕ
precision = read ⊳ (char '.' ⋫ many digit)

----------------------------------------

{- | whether to format a bytes value in terms of powers of 10^3, or 2^10 -}
data ByteFmtBase = B_1000 | B_1024 deriving (Eq)

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
                (pfx,exp) ∷ (𝕄 ℂ, Word8)= case ex of
                              0 → (𝕹,  0)
                              1 → (𝕵 'k', 1)
                              2 → (𝕵 'M', 2)
                              3 → (𝕵 'G', 3)
                              4 → (𝕵 'T', 4)
                              5 → (𝕵 'P', 5)
                              6 → (𝕵 'E', 6)
                              7 → (𝕵 'Z', 7)
                              _ → (𝕵 'Y', 8)
                formatB n = fixed n % Formatters.char % Formatters.string % "B"
                i = if b ≡ B_1024 then "i" else ""
             in case pfx of
                 𝕹 → sformat (int % "B") bytes
                 𝕵 c  → let mant = fromIntegral bytes / (x^exp)
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
  toUTCTimeY = 𝕵

instance ToUTCTimeY (𝕄 UTCTime) where
  toUTCTimeY = id

{- | Format a (Maybe UTCTime), in almost-ISO8601-without-fractional-seconds
     (always in Zulu). -}
formatUTCY ∷ ToUTCTimeY α ⇒ α → 𝕋
formatUTCY mt = case toUTCTimeY mt of
                  𝕵 t → pack $ formatTime defaultTimeLocale "%FZ%T" t
                  𝕹   → "-------------------"

{- | Format a (Maybe UTCTime), in ISO8601-without-fractional-seconds (always in
     Zulu), with a leading 3-letter day-of-week. -}
formatUTCYDoW ∷ ToUTCTimeY α ⇒ α → 𝕋
formatUTCYDoW mt = case toUTCTimeY mt of
                     𝕵 t → pack $ formatTime defaultTimeLocale "%FZ%T %a" t
                     𝕹   → "-----------------------"

toFormatUTC ∷ ToUTCTimeY α ⇒ Format ρ (α → ρ)
toFormatUTC = later $ LazyBuilder.fromText ∘ formatUTCY

toFormatUTCDoW ∷ ToUTCTimeY α ⇒ Format ρ (α → ρ)
toFormatUTCDoW = later $ LazyBuilder.fromText ∘ formatUTCYDoW

----------------------------------------

renderStackLine ∷ (𝕊,SrcLoc) → 𝕊
renderStackLine (fname,loc) = let to x y = x ◇ "→" ◇ y
                                  toS x y = to (show x) (show y)
                                  col l c = l ◇ "[" ◇ c ◇ "]"
                                  colS l c = col (show l) (show c)
                                  pkg = srcLocPackage   loc
                                  mdl = srcLocModule    loc
                                  fn  = srcLocFile      loc
                                  sc  = srcLocStartCol  loc
                                  sl  = srcLocStartLine loc
                                  ec  = srcLocEndCol    loc
                                  el  = srcLocEndLine   loc
                                  st  = colS sl sc
                                  ed  = colS el ec
                                  src = ю [ pkg, ":", mdl, ":" ◇ fn ]
                                  lc = if sl ≡ el
                                       then ю [ col (show sl) (sc `toS` ec) ]
                                       else st `to` ed
                               in ю [ "«", fname, "»", " (", src, "#", lc, ")" ]

----------------------------------------

formatStackHead ∷ HasCallstack α ⇒ α → 𝕊
formatStackHead a = case getCallStack (a ⊣ callstack) of
                      []      → "«NO STACK»"
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
    Failure e    → error $ show e
    Success toks → appE (varE fnam) $ foldr conjoin emptyStr (fmap tokOp toks)
                   where conjoin  = infixOp '(%)
                         emptyStr = litE $ stringL ""

{- | Implement a token.  Regular strings pass through; conversions ("%…") are
     implemented, and padded as necessary.
     Conversion token as formatter; e.g., %-3t ⇒ (left 3 ' ') %. stext
 -}
tokOp ∷ Token → ExpQ
-- literal string
tokOp (Str s) = litE $ stringL s
-- conversion, no padding
tokOp (Conversion mdl   -- ^ ∷ Modifier - MOD_COMMIFY | MOD_NONE,
                        --                e.g., MOD_COMMIFY in %,9d
                  fill_ -- ^ ∷ (𝕄(ℤ,ℂ)) - fill width & char, e.g., (3,'0') in
                        --                %03.2f
                  prec  -- ^ ∷ 𝕄 ℕ      - precision, e.g., 2 in %3.2f
                  txt   -- ^ ∷ 𝕄 𝕋      - string option, e.g., "xx" in %3{xx}f
                  convc -- ^ ∷ ℂ        - conversion char, e.g., 'f' in %3f
      ) =

  let CharOp op = Map.findWithDefault badconv convc charOps
                  where badconv = error $ "bad conversion char '" ◇ [convc] ◇"'"
      t = op convc mdl (fst ⊳ fill_) prec txt
      (w,f) = fromMaybe (0,'!') fill_
  in
    if mdl ∈ [MOD_NONE,MOD_COLON] ∨ convc ∈ "dfnxboe"
    then infixOp '(%.) (fillOp (w,f,mdl)) t
    else error $ "commafication not available with conv '" ◇ [convc] ◇"'"

----------------------------------------

{- create a fill expression for simple left/right fills (no commafication) -}
fillIt ∷ Name → ℕ → ℂ → ExpQ
fillIt direction width c =
--  appE (appE (varE direction) (litE (integerL width))) (litE $ charL c)
  appE (appE (varE direction) (appE (varE 'fromInteger) [| width|])) (litE $ charL c)

fillIt' ∷ Name → ℕ → ℂ → ExpQ
fillIt' f n c =
  appE (varE 'buildLTFormatter)
--       (appE (appE (varE f) (litE $ charL c)) (litE (integerL n)))
       (appE (appE (varE f) (litE $ charL c)) (appE (varE 'fromInteger) [| n|]))

{- | Transform a `LT` transformer to a `Builder`. -}
buildLTTrans ∷ Buildable ρ ⇒
               (LT.Text → LT.Text) → ρ → LazyBuilder.Builder
buildLTTrans f =
  LazyBuilder.fromLazyText ∘ f ∘ LazyBuilder.toLazyText ∘ Buildable.build

buildLTFormatter ∷ Buildable ρ ⇒
                   (LT.Text → LT.Text) → Format α (ρ → α)
buildLTFormatter = later ∘ buildLTTrans

{- | Apply a text transformation to each line of a piece of text. -}
eachLine ∷ Buildable ρ ⇒ (LT.Text → LT.Text) → Format α (ρ → α)
eachLine f =
  -- we split & intercalate rather than lines/unlines, because the latter is
  -- lossy where the last "line" does or does not end in a newline
  buildLTFormatter $ LT.intercalate "\n" ∘ fmap f ∘ LT.split (≡'\n')

{- | Pad out each line to (to the left) a given width with a given character. -}
lefts ∷ Buildable ρ ⇒ ℕ → ℂ → Format α (ρ → α)
lefts k c = eachLine (LT.justifyRight (fromIntegral k) c)

{-| This will only work with numbers… -}
commify ∷ ℂ → ℤ → LT.Text → LT.Text
commify c i t =
  let len = fromIntegral ∘ LT.length
  in  case LT.breakOn "e" t of
        (_,"") → -- no scientific notation
                 case LT.breakOn "." t of
                   (_,"") → -- pure integer
                            commifyL c i t
                   (l,r)  → -- has decimal point
                            let r' = commifyR c 0 (LT.tail r)
                            in  commifyL c (max 0 $ i - len r' - 1) l ◇ "." ◇ r'
        (m,e) → -- scientific notation
                let e' = commifyL '¡' {-^ pad shouldn't matter -} 0 (LT.tail e)
                    m' = commify c (max 0 $ i - len e' - 1) m
                 in m' ◇ "e" ◇ e'

{-| Insert a comma inbetween every three digits, from the right.
    If `i` is non-zero, the result will have `c`s added to ensure the minimum
    width.  If 'c' ≡ '0', and i > 0, that padding will be subject to
    commification. Note that the final pad group may have four '0's, to avoid
    leading with a comma.
-}
commifyL ∷ ℂ → ℤ → LT.Text → LT.Text
commifyL c i t =
  let
    t' = -- t, commified (from the right, working left; as is standard with
         -- integers)
         LT.intercalate "," $ LT.reverse ⊳ reverse(LT.chunksOf 3 $ LT.reverse t)
    i' = fromIntegral i
  in
    if c ≡ '0' ∧ i > 0
    then if fromIntegral (LT.length t') < i
         then let c'  = LT.singleton c
                  c'' = LT.replicate 3 c'
                  s   = if c ≡ ' ' then " " else ","
                  p   = LT.takeWhile isDigit t'
                  p'  = s ◇ LT.replicate (3-LT.length p) c'
                  t'' = LT.takeEnd i' $ LT.replicate i' (s ◇ c'') ◇ p' ◇ t'
              in  if ',' ≡ LT.head t''
                  then c' ◇ LT.tail t''
                  else t''
         else t'
    else if i < 0
         then LT.justifyLeft  i' c t'
         else LT.justifyRight i' c t'

{-| Rightwards commify, for use after a decimal point. -}
commifyR ∷ ℂ → ℕ → LT.Text → LT.Text
commifyR c {-^ pad character -} i {-^ expected output width, incl. commas -} t =
  let
    t' = LT.intercalate "," (LT.chunksOf 3 t)
    tke = LT.take ∘ fromIntegral
    replicat = LT.replicate ∘ fromIntegral
  in
    if fromIntegral (LT.length t') < i
    then let c'  = LT.singleton c
             c'' = LT.replicate 3 c'
             s   = if c ≡ ' ' then " " else ","
             p   = LT.takeWhileEnd isDigit t'
             p'  = LT.replicate (3-LT.length p) c' ◇ s
             t'' = tke i $ t' ◇ p' ◇ replicat i c''
         in  if ',' ≡ LT.last t''
             then LT.init t'' ◇ c'
             else t''
    else t'

{- | Pad out each line to (to the right) a given width with a given character.-}
rights ∷ Buildable ρ ⇒ ℕ → ℂ → Format α (ρ → α)
rights k c = eachLine (LT.justifyLeft (fromIntegral k) c)

-- | conversion fill; -x → left, (+)x → right

fillOp ∷ (ℤ,ℂ,Modifier) → ExpQ
fillOp (i,c,m) =
  if m ≡ MOD_COMMIFY
  then fillIt' 'commify (abs i) c
  else
    if i < 0
    then fillIt 'rights (abs i) c
    else fillIt 'lefts  (fromInteger     i)  c

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

   Some numeric specificiers may have a ',' preceding the pad (if any), to cause
   the number to have a comma inserted every three digits.  If there is a
   positive pad value, and the pad character is '0', then that is also subject
   to commification.  In this instance, the first four characters may be a '0' -
   we elide the comma there, to ensure that the first character is not itself
   a comma.

   Where noted below, some specifiers also allow a precision (after a 'decimal
   point').

   [@L@] - A `Foldable` of things, where the things are instances of
           `Printable`, joined with ',', thus
           @ (`Foldable` φ, Printable τ) ⇒ φ intercalate "," (fmap toText τ) @

   [@l@] - LazyText `LT.Text`

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

   [@y@] - `Integral` α ⇒ Render as bytes, with a 10^3 multiplier.  This tries
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

   [@m@] - `Real` α    ⇒ render as a timespan; e.g., 1s
-}

{- | Character op: non-𝕹 precision causes error. -}
charOpNoPrecision ∷ ExpQ → ℂ → 𝕄 ℕ → 𝕄 𝕋 → ExpQ
charOpNoPrecision f _ 𝕹 𝕹 = f
charOpNoPrecision _ chr (𝕵 prec) 𝕹 =
  error $ ю [ "conversion char '", [chr], "' does not admit precision ("
            , show prec, ")" ]
charOpNoPrecision _ chr 𝕹 (𝕵 t) =
  error $ ю [ "conversion char '", [chr], "' admits no text ({", unpack t,"})" ]
charOpNoPrecision _ chr (𝕵 prec) (𝕵 t) =
  error $ ю [ "conversion char '", [chr], "' admits neither precision ("
            , show prec, ")", " nor text ({", unpack t
            , "})" ]

------------------------------------------------------------

-- second tuple member is whether commafication is supported
-- function args:
--   ) conversion character
--   ) modifier (Commify, or None)
--   ) fill width
--   ) precision, e.g., 2 in %3.2f
--   ) string option, e.g., "xx" in %3{xx}f
newtype CharOp = CharOp (ℂ -> Modifier -> (𝕄 ℤ) -> (𝕄 ℕ) -> (𝕄 𝕋) -> ExpQ)

----------------------------------------

{- | Conversion character as formatter; e.g., 't' → stext; takes fill width &
     precision too, lest that affect the conversion. -}
charOps ∷ Map.Map ℂ CharOp
charOps = Map.fromList $
  let
    no_prec f = CharOp $ \ c _ _ p t → charOpNoPrecision f c p t
    e_no_text c t = error $ "conversion char '" ◇ [c] ◇ "' "
                          ◇ "admits no text ({" ◇ unpack t ◇ "})"
  in
    [ -- list (foldable), joined with ','
      ('L', no_prec ⟦ toTextListF ⟧)
      -- lazy text
    , ('l', no_prec ⟦ text ⟧)
    , ('s', no_prec ⟦ Formatters.string ⟧)
    , ('t', no_prec ⟦ stext ⟧)
    , ('T', no_prec ⟦ toTextF ⟧)
    , ('q', no_prec ⟦ toShell ⟧)
    , ('w', no_prec ⟦ shown ⟧)

      -- list (foldable) of shell-quoted things, joined with ' '
    , ('Q', no_prec ⟦ toShellList ⟧)

    , ('d', no_prec ⟦ int ⟧)
    , ('x', no_prec ⟦ hex ⟧)
    , ('b', no_prec ⟦ bin ⟧)
    , ('o', no_prec ⟦ oct ⟧)
    , ('n', no_prec ⟦ tonum ⟧)

    , let char_op _ _ _ 𝕹     𝕹     = ⟦ floatmin ⟧
          char_op _ _ _ (𝕵 i) 𝕹     = ⟦ fixed i ⟧
          char_op c _ _ _     (𝕵 t) = e_no_text c t
      in  ('f', CharOp char_op)
    , let char_op _ _ _ 𝕹     𝕹     = ⟦ expt 0 ⟧
          char_op _ _ _ (𝕵 i) 𝕹     = ⟦ expt i ⟧
          char_op c _ _ _     (𝕵 t) = e_no_text c t
      in  ('e', CharOp char_op)

    , ('y', no_prec ⟦ toFormatBytes B_1000 ⟧)
    , ('Y', no_prec ⟦ toFormatBytes B_1024 ⟧)

    , ('z', no_prec ⟦ toFormatUTC ⟧)
    , ('Z', no_prec ⟦ toFormatUTCDoW ⟧)

    , ('k', no_prec ⟦ toFormatStackHead ⟧)
    , ('K', no_prec ⟦ toFormatCallStack ⟧)

    , let char_op _ m _ p _ = [| fmtTime m p |] in ('m',CharOp char_op)
    ]

----------------------------------------

floatmin ∷ Real α ⇒ Format r (α → r)
floatmin = let dropper = dropWhileEnd (`elem` (".0" ∷ 𝕊))
            in later $ LazyBuilder.fromText ∘ dropper ∘ sformat shortest

----------------------------------------

tonum ∷ ToNum α ⇒ Format r (α → r)
tonum = mapf toNumI int

----------------------------------------

expt ∷  RealFloat α ⇒ ℕ → Format r (α → r)
expt n = later (\ f →
  let (m,e ∷ ℤ) = decompose f
   in LazyBuilder.fromText $ (sformat $ (fixed n % "e" % int)) m e)

----------------------------------------

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
infixOp op l r = infixE (𝕵 l) (varE op) (𝕵 r)

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

instance FormatTarget LT.Text where
  output = format

instance FormatTarget 𝕊 where
  output = formatToString

------------------------------------------------------------

{- Given a list of lines, each being a list of columns; pad out the columns
   to provide an aligned display.

   The columns are padded out according to the input `pads` argument.  Widths
   are set according to the widest input column.  Columns for which no justify
   value is provided are left unmolested.
-}
data Justify = JustifyLeft | JustifyRight

-- provide fixed width args, and ignore args, and centrejustify args

columnify ∷ [Justify] → [[𝕋]] → [[𝕋]]
columnify pads zs =
  let pad_t ∷ ℤ → 𝕋 → 𝕋
      pad_t (unNegate → (SignMinus,n)) t = replicate @𝕋 (n ⊖ length t) ' ' ◇ t
      pad_t (unNegate → (SignPlus, n)) t = t ◇ replicate @𝕋 (n ⊖ length t) ' '

      col_widths = transpose zs & each ⊧ (\ ys → maximumDef 0 $ length ⊳ ys)
      xx JustifyLeft  = 1
      xx JustifyRight = (-1)
      col_widths' = (\(x,y) → fromIntegral y * (xx  x)) ⊳ zip pads col_widths
  in
    (^.. each) ∘ (zipWith pad_t (col_widths' ◇ repeat 0)) ⊳ zs

-- that's all, folks! ---------------------------------------------------------
