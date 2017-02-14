{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Text.Fmt
  ( ByteFmtBase(..)
  , fmt, fmtS, fmtL, fmtT, formatBytes
  -- for testing only
  , Token(..), conversion, fill, sprintf, tokens )
where

import Prelude ( Double, Integer, Integral, (/), (^)
               , abs, error, floor, fromIntegral, toInteger )

-- base --------------------------------

import Control.Applicative    ( (<*), (<*>), (*>), (<|>), many )
import Data.Char              ( Char, toUpper )
import Data.Either            ( Either( Left, Right ) )
import Data.Eq                ( Eq, (==) )
import Data.Foldable          ( Foldable, foldr1, toList )
import Data.Function          ( (.), ($), const )
import Data.Functor           ( (<$>), fmap )
import Data.List              ( (++), concat )
import Data.Maybe             ( Maybe( Just, Nothing ) )
import Data.Ord               ( (<), (>) )
import Data.String            ( String )
import Data.Word              ( Word8 )
import Numeric                ( logBase )
import Numeric.Natural        ( Natural )
import Text.Read              ( read )
import Text.Show              ( Show( show ) )

-- formatting --------------------------

import qualified  Formatting.Formatters  as  Formatters

import Formatting             ( Format, (%), (%.)
                              , format, formatToString, later, sformat )
import Formatting.Formatters  ( bin, expt, fixed, float, hex, int, left
                              , oct, right, shown, stext, text )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit, noneOf, oneOf, string )
import Text.Parsec.Combinator  ( eof, many1, option, optionMaybe )
import Text.Parsec.Error       ( ParseError )
import Text.Parsec.Prim        ( ParsecT, Stream, (<?>), parse, try )

-- template-haskell --------------------

import Language.Haskell.TH  ( ExpQ, Name, appE, charL, conE, infixE, integerL
                            , litE, stringL, varE )
import Language.Haskell.TH.Quote
                            ( QuasiQuoter( QuasiQuoter, quoteDec
                                         , quoteExp, quotePat, quoteType ) )

-- text --------------------------------

import qualified  Data.Text.Lazy          as  LazyText
import qualified  Data.Text.Lazy.Builder  as LazyBuilder

import Data.Text  ( Text, intercalate, pack, unpack )


-- textconv ----------------------------

import Data.Text.Conv  ( ToText( toText ) )

-------------------------------------------------------------------------------

data Token = -- | a conversion specifier, e.g., %3.2f; the first value is the
             --   fill, if any - the width, and the char (default ' ', may be
             --   '0')
             Conversion (Maybe (Integer, Char)) (Maybe Natural) Char
           | Str String
  deriving (Eq, Show)

-- | tokenize a string into strings & conversions
tokens :: Text -> Either ParseError [Token]
tokens s = concatTokens <$> parse (tokenP <* eof) (unpack s) s

----------------------------------------

-- | squish consecutive Str together

concatTokens :: [Token] -> [Token]
concatTokens (Str s : Str s' : ts) = concatTokens (Str (s ++ s') : ts)
concatTokens (t : t' : ts)         = t : t' : ts
concatTokens ts                    = ts

----------------------------------------

-- | parse a string into tokens
tokenP :: Stream s m Char => ParsecT s u m [Token]
tokenP = many (simpleStr <|> try escapePC <|> conversion)

----------------------------------------

-- | parse a string intoa conversion specifier

conversion :: Stream s m Char => ParsecT s u m Token
conversion =
  Conversion <$> (string "%" *> optionMaybe fill)
             <*> optionMaybe precision
             <*> (oneOf "bdeflLostTwxyY" <?> "valid conversion char")

----------------------------------------

-- | parser for the fill spec of a conversion (the -07 of "%-07s", for example)
fill :: Stream s m Char => ParsecT s u m (Integer, Char)
fill =
  (\ a b c d -> (read (concat [a,[c],d]), b)) <$> option "" (string "-")
                                              <*> option ' ' (char '0')
                                              <*> oneOf "123456789"
                                              <*> many digit

----------------------------------------

-- | parse for the precision part of a conversion (.2 of "%3.2f", for example)

precision :: Stream s m Char => ParsecT s u m Natural
precision = read <$> (char '.' *> many digit)

----------------------------------------

-- | parser for an unadorned string (without any % chars)
simpleStr :: Stream s m Char => ParsecT s u m Token
simpleStr = Str <$> many1 (noneOf "%")

----------------------------------------

-- | parser for an escaped '%' (represented in the incoming string as "%%")
escapePC :: Stream s m Char => ParsecT s u m Token
escapePC = Str <$> const "%" <$> string "%%"

----------------------------------------

data ByteFmtBase = B_1000 | B_1024
  deriving Eq

-- | try really hard to fit within 7 chars
formatBytes :: Integral a => ByteFmtBase -> a -> Text
formatBytes _ (toInteger -> 0) = "0"
formatBytes b bs =
    case b of
      B_1000 -> go 1000 bs -- (byteSize bs)
      B_1024 -> go 1024 bs -- (fromIntegral $ byteSize bs)
    where go :: Integral b => Double -> b -> Text
          go x bytes =
            let ex :: Word8 = floor (logBase x $ fromIntegral bytes)
                (pfx,exp) :: (Maybe Char, Word8)= case ex of
                              0 -> (Nothing,  0)
                              1 -> (Just 'k', 1)
                              2 -> (Just 'M', 2)
                              3 -> (Just 'G', 3)
                              4 -> (Just 'T', 4)
                              5 -> (Just 'P', 5)
                              6 -> (Just 'E', 6)
                              7 -> (Just 'Z', 7)
                              _ -> (Just 'Y', 8)
             in case pfx of
                 Nothing -> sformat (int % "B") bytes
                 Just c  -> let mant = (fromIntegral bytes) / (x^exp)
                             in if mant < 10
                                then -- [fmt|%3.2f%T%sB|]
                                     sformat (fixed 2 % Formatters.char % Formatters.string % "B")
                                     mant (if b == B_1024 then toUpper c else c)
                                          (if b == B_1024 then "i" else "")
                                else if mant < 100
                                     then -- [fmt|%4.1f%T%sB|]
                                          sformat (fixed 1 % Formatters.char % Formatters.string % "B")
                                          mant (toUpper c) (if b == B_1024 then "i" else "")
                                     else -- [fmt|%4f%T%sB|]
                                          sformat (fixed 0 % Formatters.char % Formatters.string % "B")
                                          mant (toUpper c) (if b == B_1024 then "i" else "")


----------------------------------------

-- | parse a fmt, return an ExpQ that when spliced, takes arguments to pass
--   to the formatter to provide a textlike thing (see `FormatTarget`)
sprintf :: Text -> ExpQ
sprintf = sprintf_ 'output

-- | like `sprintf`, but always produces a String (to reduce scoped type
--   variables)
sprintfS :: Text -> ExpQ
sprintfS = sprintf_ 'formatToString

sprintfT :: Text -> ExpQ
sprintfT = sprintf_ 'sformat

sprintfL :: Text -> ExpQ
sprintfL = sprintf_ 'format

sprintf_ :: Name -> Text -> ExpQ
sprintf_ fnam t =
  case tokens t of
    Left  e    -> error $ show e
    Right toks -> appE (varE fnam) $ foldr1 conjoin (fmap tokOp toks)
                  where conjoin = infixOp '(%)

-- | conversion token as formatter; e.g., %-3t => (left 3 ' ') %. stext
tokOp :: Token -> ExpQ
tokOp (Str s)                       = litE $ stringL s
tokOp (Conversion Nothing p c)      = charOp c Nothing p
tokOp (Conversion (Just (i,c)) p o) =
  infixOp '(%.) (fillOp (i,c)) (charOp o (Just i) p)
-- tokOp (Conversion x y)         =
--   error $ "failed conversion: " ++ show x ++ ", " ++ show y

----------------------------------------

-- create a fill expression
fillIt :: Name -> Integer -> Char -> ExpQ
fillIt direction width chr =
  appE (appE (varE direction) (litE (integerL width))) (litE $ charL chr)

-- | conversion fill; -x -> left, (+)x -> right

fillOp :: (Integer,Char) -> ExpQ
fillOp (i,c) | i < 0 = fillIt 'right (abs i) c
fillOp (i,c) | i > 0 = fillIt 'left       i  c
fillOp (_,_) = -- i == 0 : something's gone wrong!
               error "cannot fill with size 0"

----------------------------------------

toTextF :: ToText t => Format r (t -> r)
-- toTextF = later $ LazyBuilder.fromLazyText . toLazyText
toTextF = later $ LazyBuilder.fromText . toText

toTextListF :: (Foldable f, ToText t) => Format r (f t -> r)
toTextListF =
  later $
--    LazyBuilder.fromLazyText . LazyText.intercalate "," .
    LazyBuilder.fromText . intercalate "," .
--      fmap toLazyText . toList
      fmap toText . toList

toFormatBytes :: Integral a => ByteFmtBase -> Format r (a -> r)
toFormatBytes b = later $ LazyBuilder.fromText . formatBytes b

-- | conversion character as formatter; e.g., 't' -> stext; takes precision
--   too, lest that affect the conversion
charOp :: Char -> Maybe Integer -> Maybe Natural -> ExpQ
charOp 'L' _ _ = varE 'toTextListF
charOp 'l' _ _ = varE 'text
charOp 's' _ _ = varE 'Formatters.string
charOp 't' _ _ = varE 'stext
charOp 'T' _ _ = varE 'toTextF
charOp 'w' _ _ = varE 'shown

charOp 'd' _ _ = varE 'int
charOp 'x' _ _ = varE 'hex
charOp 'b' _ _ = varE 'bin
charOp 'o' _ _ = varE 'oct

charOp 'f' _ Nothing  = varE 'float
charOp 'f' _ (Just i) = appE (varE 'fixed) (litE (integerL $ fromIntegral i))
charOp 'e' _ Nothing  = appE (varE 'expt) (litE (integerL 0))
charOp 'e' _ (Just i) = appE (varE 'expt) (litE (integerL $ fromIntegral i))

charOp 'y' _ Nothing = appE (varE 'toFormatBytes) (conE 'B_1000)
charOp 'y' _ (Just _) = error $ "y format does not handle precision"
charOp 'Y' _ Nothing = appE (varE 'toFormatBytes) (conE 'B_1024)
charOp 'Y' _ (Just _) = error $ "Y format does not handle precision"

charOp x _ p = let errPfx = "bad conversion char or unhandled precision: '"
                in error $ errPfx ++ [x] ++ "' (" ++ show p ++ ")"

----------------------------------------

-- | infix a function between two values
infixOp :: Name -> ExpQ -> ExpQ -> ExpQ
infixOp op l r = infixE (Just l) (varE op) (Just r)

----------------------------------------

fmt :: QuasiQuoter
fmt =  QuasiQuoter { quoteDec  = error "not implemented"
                   , quoteType = error "not implemented"
                   , quotePat  = error "not implemented"
                   , quoteExp  = sprintf . pack
                   }

--------------------

fmtS :: QuasiQuoter
fmtS =  QuasiQuoter { quoteDec  = error "not implemented"
                    , quoteType = error "not implemented"
                    , quotePat  = error "not implemented"
                    , quoteExp  = sprintfS . pack
                    }

--------------------

fmtL :: QuasiQuoter
fmtL =  QuasiQuoter { quoteDec  = error "not implemented"
                    , quoteType = error "not implemented"
                    , quotePat  = error "not implemented"
                    , quoteExp  = sprintfL . pack
                    }

--------------------

fmtT :: QuasiQuoter
fmtT =  QuasiQuoter { quoteDec  = error "not implemented"
                    , quoteType = error "not implemented"
                    , quotePat  = error "not implemented"
                    , quoteExp  = sprintfT . pack
                    }

------------------------------------------------------------

class FormatTarget t where
  output :: Format t a -> a

instance FormatTarget Text where
  output = sformat

instance FormatTarget LazyText.Text where
  output = format

instance FormatTarget String where
  output = formatToString

-- that's all, folks! ---------------------------------------------------------
