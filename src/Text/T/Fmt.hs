{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK hide #-}

{- | tests for Text.Fmt -}

module Text.T.Fmt
  ( _test
  , tests
  ) where

import Base0T
import Prelude ( Double, Float, Int, Integer, (^) )

-- base --------------------------------

import Data.Ratio      ( (%) )
import Data.String     ( unlines )
import GHC.Stack       ( SrcLoc(SrcLoc), fromCallSiteList )
import Numeric.Natural ( Natural )

-- base-unicode-symbols ----------------

import Prelude.Unicode ( ℚ )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool   ( pattern 𝕱 )
import Data.MoreUnicode.Either ( 𝔼, pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Lens   ( (⩼) )
import Data.MoreUnicode.Maybe  ( pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monoid ( ф )
import Data.MoreUnicode.String ( 𝕊 )
import Data.MoreUnicode.Text   ( 𝕋 )

-- prettyprinter -----------------------

import Prettyprinter.Internal    ( defaultLayoutOptions, layoutPretty )
import Prettyprinter.Render.Text ( renderStrict )

-- tasty -------------------------------

import Test.Tasty ( defaultMain )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit ( Assertion, assertBool, (@?=) )

-- text --------------------------------

import Data.Text.Lazy qualified as LT

import Data.Text ( Text, intercalate, isInfixOf, unpack )

-- text-printer ------------------------

import Text.Printer qualified as P

-- time --------------------------------

import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

-- trifecta ----------------------------

import Text.Trifecta.Parser ( parseString )
import Text.Trifecta.Result ( ErrInfo, Result(Failure, Success), _Success,
                              _errDoc )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Text.Fmt       ( ByteFmtBase(B_1000, B_1024), commify, commifyR,
                        conversion, fill, fmt, fmtL, fmtS, fmtT, formatBytes,
                        sprintf, tokens )
import Text.Fmt.Token ( Modifier(MOD_COMMIFY, MOD_NONE),
                        Token(Conversion, Str) )

-------------------------------------------------------------------------------

data TestToText = TestToText Text

instance Printable TestToText where
  print (TestToText t) = P.text $ "ttt: " ⊕ t

ts ∷ [TestToText]
ts =  [ TestToText "c", TestToText "b", TestToText "a" ]

-- | run the tests
_test ∷ IO ()
_test = defaultMain tests

tests ∷ TestTree
tests = testGroup "Text.Fmt" [ fillTest, convTest, formatBytesTest
                             , tokensTest, sprintfTest, commifyTests
                             , commifyRTests, fmtTest
                             ]

convTest ∷ TestTree
convTest =
  testGroup "conversion" $
    let testConv t expect =
          testCase t $ parseString conversion ф t ⩼ _Success @?= 𝕵 expect
      in [ testConv "%t"      (Conversion MOD_NONE 𝕹 𝕹 𝕹 't')
         , testConv "%-03.2f" (Conversion MOD_NONE (𝕵 (-3,'0')) (𝕵 2) 𝕹 'f')
         ]

commifyTests ∷ TestTree
commifyTests =
  let checkn n exp t = testCase (LT.unpack exp) $ exp @=? commify '0' n t
      check0 = checkn 0
      check6 = checkn 6
      check8 = checkn 8
  in  testGroup "commify"
        [ check0            ""          ""
        , check0           "1"         "1"
        , check0          "21"        "21"
        , check0         "321"       "321"
        , check0       "4,321"      "4321"
        , check0      "54,321"     "54321"
        , check0     "654,321"    "654321"
        , check0   "7,654,321"   "7654321"
        , check0  "87,654,321"  "87654321"
        , check0 "987,654,321" "987654321"
        , check6      "00,000"          ""
        , check6      "00,001"         "1"
        , check6      "00,021"        "21"
        , check6      "00,321"       "321"
        , check6      "04,321"      "4321"
        , check6      "54,321"     "54321"
        , check6     "654,321"    "654321"
        , check6   "7,654,321"   "7654321"
        , check6  "87,654,321"  "87654321"
        , check6 "987,654,321" "987654321"
        , check8    "0000,000"          ""
        , check8    "0000,001"         "1"
        , check8    "0000,021"        "21"
        , check8    "0000,321"       "321"
        , check8    "0004,321"      "4321"
        , check8    "0054,321"     "54321"
        , check8    "0654,321"    "654321"
        , check8   "7,654,321"   "7654321"
        , check8  "87,654,321"  "87654321"
        , check8 "987,654,321" "987654321"
        , testCase "7777 (9)"  $ "0,007,777"   @=? commify '0' 9 "7777"
        , testCase "7777.7777" $ "7,777.777,7" @=? commify '0' 0 "7777.7777"
        , testCase "7777.7777" $ "0007,777.777,7" @=? commify '0' 14 "7777.7777"
        , testCase "10.00e5000" $ "10.00e5,000" @=? commify '0' 0 "10.00e5000"
        , testCase "10.00e6000" $
            "0,010.00e6,000" @=? commify '0' 14 "10.00e6000"
        , testCase "10.00e7000" $ "   10.00e7,000" @=? commify ' ' 14 "10.00e7000"
        ]

commifyRTests ∷ TestTree
commifyRTests =
  let checkn n exp t = testCase (LT.unpack exp) $ exp @=? commifyR '0' n t
      check0 = checkn 0
      check6 = checkn 6
      check8 = checkn 8
  in  testGroup "commifyR"
        [ check0 ""            ""
        , check0 "1"           "1"
        , check0 "21"          "21"
        , check0 "321"         "321"
        , check0 "432,1"       "4321"
        , check0 "543,21"      "54321"
        , check0 "654,321"     "654321"
        , check0 "765,432,1"   "7654321"
        , check0 "876,543,21"  "87654321"
        , check0 "987,654,321" "987654321"
        , check6 "000,00"      ""
        , check6 "100,00"      "1"
        , check6 "210,00"      "21"
        , check6 "321,00"      "321"
        , check6 "432,10"      "4321"
        , check6 "543,21"      "54321"
        , check6 "654,321"     "654321"
        , check6 "765,432,1"   "7654321"
        , check6 "876,543,21"  "87654321"
        , check6 "987,654,321" "987654321"
        , check8 "000,0000"    ""
        , check8 "100,0000"    "1"
        , check8 "210,0000"    "21"
        , check8 "321,0000"    "321"
        , check8 "432,1000"    "4321"
        , check8 "543,2100"    "54321"
        , check8 "654,3210"    "654321"
        , check8 "765,432,1"   "7654321"
        , check8 "876,543,21"  "87654321"
        , check8 "987,654,321" "987654321"
        ]

formatBytesTest ∷ TestTree
formatBytesTest =
  let (^^) ∷ Int → Int → Int
      x ^^ y = x ^ y
      testBy ∷ Int → Text → TestTree
      testBy v ex = testCase (show v ⊕ "b") $ formatBytes B_1000 v @?= ex
      testBi ∷ Int → Text → TestTree
      testBi v ex = testCase (show v ⊕ "b") $ formatBytes B_1024 v @?= ex
   in testGroup "formatBytes" $
        [ testBi 0 "0"
        , testBy 0 "0"
        , testBy 500 "500B"
        , testBi 500 "500B"
        , testBy 1000 "1.00kB"
        , testBi 1000 "1000B"
        , testBy 1024 "1.02kB"
        , testBi 1024 "1.00KiB"
        , testBy 5000 "5.00kB"
        , testBi 5000 "4.88KiB"
        , testBy 1000000 "1.00MB"
        , testBi 1000000 "977KiB"
        , testBy 1048576 "1.05MB"
        , testBi 1048576 "1.00MiB"
        , testBy (1024^^3) "1.07GB"
        , testBi (1024^^3) "1.00GiB"
        , testBi (1024^^3) "1.00GiB"
        ]

----------------------------------------

fillTest ∷ TestTree
fillTest = testGroup "fill" $
  let testFill  s i = testCase s $ parseString fill ф s ⩼ _Success @?= 𝕵 i
      -- testFillE s e = testCase s $ first show (parseInt s) @?= 𝕷 e
   in [ testFill "-7" (-7, ' ')
      , testFill "7"  (7, ' ')
      ]

{-| Extract error text from an ErrInfo. -}
eiText ∷ ErrInfo → 𝕋
-- layoutCompact splits the message string into separate lines
eiText = renderStrict ∘ layoutPretty defaultLayoutOptions ∘ _errDoc

resultToStr ∷ Show α ⇒ Result α → 𝕊
resultToStr (Success a) = "Success: " ⊕ show a
resultToStr (Failure e) = "Failure: " ⊕ unpack (eiText e)

cmp ∷ (Eq α, Show α) ⇒ Result α → 𝔼 𝕋 α → Assertion
cmp got exp =
  let emsg = unlines ["expected: " ⊕ either unpack show exp
                     , "got: " ⊕ resultToStr got]
  in  case (got,exp) of
        (Failure l, 𝕷 r) → assertBool emsg $ r `isInfixOf` (eiText l)
        (Success l, 𝕽 r) → l @?= r
        (_,_)            → assertBool emsg 𝕱

tokensTest ∷ TestTree
tokensTest =
  let testTokens ∷ 𝕋 → 𝔼 𝕋 [Token] → TestTree
      testTokens s expect = testCase (unpack s) $
                              (tokens s) `cmp` expect
   in testGroup "tokens"
         [ testTokens "just a string"    (𝕽 [ Str "just a string" ])
         , testTokens ""                 (𝕽 [ ])
         , testTokens "percent %% here"  (𝕽 [ Str "percent % here" ])
         , testTokens "percent after %%" (𝕽 [ Str "percent after %" ])
         , testTokens "%%percent before" (𝕽 [ Str "%percent before" ])
         , testTokens "%"                (𝕷 "unexpected EOF")
         , testTokens "%%%"              (𝕷 "unexpected EOF")
         , testTokens "%t%"              (𝕷 "unexpected EOF")
         , testTokens "% %t"             (𝕷 "error: expected:")
         , testTokens "%\t%t"            (𝕷 "error: expected:")
         , testTokens "%t%a"             (𝕷 "error: expected:")
         , testTokens "my %ttoken"
             (𝕽 [ Str "my ", Conversion MOD_NONE 𝕹 𝕹 𝕹 't'
                    , Str "token" ])
         , testTokens "%7t token"
             (𝕽 [ Conversion MOD_NONE (𝕵 (7, ' ')) 𝕹 𝕹 't'
                    , Str " token" ])
         , testTokens "%-7t%%"
             (𝕽 [ Conversion MOD_NONE (𝕵 (-7, ' ')) 𝕹 𝕹 't'
                    , Str "%" ])
         , testTokens "%07t token"
             (𝕽 [ Conversion MOD_NONE (𝕵 (7, '0')) 𝕹 𝕹 't'
                    , Str " token" ])
         , testTokens "%-07t%%"
             (𝕽 [ Conversion MOD_NONE (𝕵 (-7, '0')) 𝕹 𝕹 't'
                    , Str "%" ])
         , testTokens "%,-07t%%"
             (𝕽 [ Conversion MOD_COMMIFY (𝕵 (-7, '0')) 𝕹 𝕹 't'
                    , Str "%" ])
         ]

sprintfTest ∷ TestTree
sprintfTest =
  testGroup "sprintf"
    [ testCase "foo"     $ $( sprintf "foo"   )          @?= ("foo"    ∷ 𝕋)
    , testCase "foo%t"   $ $( sprintf "foo%t" )    "bar" @?= ("foobar" ∷ 𝕋)
    , testCase "%tfoo"   $ $( sprintf "%tfoo" )    "bar" @?= ("barfoo" ∷ 𝕋)
    , testCase "fo%bzs"  $ $( sprintf "fo%tbz" )   "br"  @?= ("fobrbz" ∷ 𝕋)

    , testCase "%d"      $ $( sprintf "%d"  ) (  7  ∷ Int) @?= ("7"   ∷ 𝕋)
    , testCase "%d (-)"  $ $( sprintf "%d"  ) ((-7) ∷ Int) @?= ("-7"  ∷ 𝕋)
    , testCase "%dC"     $ $( sprintf "%dC" ) (  7  ∷ Int) @?= ("7C"  ∷ 𝕋)
    , testCase "F%d"     $ $( sprintf "F%d" ) (  7  ∷ Int) @?= ("F7"  ∷ 𝕋)
    , testCase "-%d"     $ $( sprintf "-%d" ) (  7  ∷ Int) @?= ("-7"  ∷ 𝕋)
    , testCase "-%d"     $ $( sprintf "-%d" ) ((-7) ∷ Int) @?= ("--7" ∷ 𝕋)
    ]

cs ∷ CallStack
cs = fromCallSiteList [ ("foo", SrcLoc "a" "b" "c" 8 13 21 34)
                      , ("bar", SrcLoc "x" "y" "z" 55 89 55 233) ]

fmtTest ∷ TestTree
fmtTest =
  let (^^) ∷ Int → Int → Int
      x ^^ y = x ^ y
      bar = "bar" ∷ String
      dayOne = posixSecondsToUTCTime 94755600
      -- HasCallStack ⇝ cite the caller, not here, for the failure position
      check ∷ HasCallStack ⇒ 𝕊 → 𝕊 → TestTree
      check exp got = testCase ("»"⊕exp⊕"«") $ got @?= exp
   in testGroup "fmt" $
    [ testCase "-empty-"  $ [fmt||]               @?= ("" ∷ 𝕋)

    , testCase "foo%tbaz" $ [fmt|foo%tbaz|] "bar" @?= ("foobarbaz" ∷ 𝕋)
    , testCase "a%3tc"    $ [fmt|a%3tc|]    "b"   @?= ("a  bc" ∷ 𝕋)
    , testCase "a%-3tc"   $ [fmt|a%-3tc|]   "b"   @?= ("ab  c" ∷ 𝕋)
    , testCase "a%-03tc"  $ [fmt|a%-03tc|]  "b"   @?= ("ab00c" ∷ 𝕋)
    , testCase "a%03tc"   $ [fmt|a%03tc|]   "b"   @?= ("a00bc" ∷ 𝕋)

    , testCase "a%-3sc"   $ [fmt|a%3sc|]    "b"   @?= ("a  bc" ∷ 𝕋)
    , testCase "a%-2lc"   $ [fmt|a%-3lc|]   "b"   @?= ("ab  c" ∷ 𝕋)
    , testCase "a%5Lc"   $ [fmt|a%5Lc|] (["b","d"] ∷ [LT.Text])
                                                  @?= ("a  b,dc" ∷ 𝕋)

    , testCase "foo%Tbaz" $ [fmt|foo%Tbaz|] bar @?= ("foobarbaz" ∷ String)

    , testCase "a%-3sc"   $ [fmt|n|]                @?= ("n" ∷ 𝕋)
    , testCase "a%-3sc"   $ [fmt|\n|]               @?= ("\n" ∷ 𝕋)
    , testCase "a%-3sc"   $ [fmt|a%3s\nc|]    "b"   @?= ("a  b\nc" ∷ 𝕋)
    , testCase "a%-3sc"   $ [fmt|a%3s\tc|]    "b"   @?= ("a  b\tc" ∷ 𝕋)
    , testCase "a%-3sc"   $ [fmt|a%3s\\ntc|]    "b"   @?= ("a  b\\ntc" ∷ 𝕋)
    , testCase "a%-3sc"   $ [fmt|a\t%3s\nc|]    "b"   @?= ("a\t  b\nc" ∷ 𝕋)


    , testCase "a%-3wc" $ [fmt|a%-3wc|] ("b" ∷ String) @?= ("a\"b\"c" ∷ 𝕋)
    , testCase "a%-5wc" $ [fmt|a%-5wc|] ("b" ∷ String) @?= ("a\"b\"  c"∷𝕋)
    , testCase "a%5wc" $ [fmt|a%5wc|] ("b" ∷ String) @?= ("a  \"b\"c" ∷ 𝕋)


    , testCase ",7"       $ [fmt|%,d|]      (7 ∷ Int) @?=         ("7" ∷ 𝕋)
    , testCase "7777"     $ [fmt|%d|]    (7777 ∷ Int) @?=      ("7777" ∷ 𝕋)
    , testCase ",7777"    $ [fmt|%,d|]   (7777 ∷ Int) @?=     ("7,777" ∷ 𝕋)
    , testCase ",777"     $ [fmt|%,09d|]  (777 ∷ Int) @?= ("0,000,777" ∷ 𝕋)
    , testCase ",7777@09" $ [fmt|%,09d|] (7777 ∷ Int) @?= ("0,007,777" ∷ 𝕋)
    , testCase ",7777@08" $ [fmt|%,08d|] (7777 ∷ Int) @?=  ("0007,777" ∷ 𝕋)
    , testCase ",7777@9"  $ [fmt|%,9d|]  (7777 ∷ Int) @?= ("    7,777" ∷ 𝕋)
    , testCase ",7777@8"  $ [fmt|%,8d|]  (7777 ∷ Int) @?=  ("   7,777" ∷ 𝕋)
    , testCase ",7777@7"  $ [fmt|%,7d|]  (7777 ∷ Int) @?=   ("  7,777" ∷ 𝕋)
    , -- yes, 7,777000 - that is, a commified 7,777; plus 0 to fill.
      -- we don't try to commify 0s on the RHS, that probably doesn't make any
      -- sense.
      testCase ",5555@-08" $ [fmt|%,-08d|] (5555 ∷ Int) @?= ("5,555000" ∷ 𝕋)
    , testCase ",7777@-06" $ [fmt|%,-06d|] (7777 ∷ Int) @?= ("7,7770" ∷ 𝕋)
    , testCase ",7777@-7" $ [fmt|%,-7d|] (7777 ∷ Int) @?= ("7,777  " ∷ 𝕋)
    , testCase ",7777@-8" $ [fmt|%,-8d|] (7777 ∷ Int) @?= ("7,777   " ∷ 𝕋)
    , testCase ",7777@-9" $ [fmt|%,-9d|] (7777 ∷ Int) @?= ("7,777    " ∷ 𝕋)

    , testCase "a%3dc: 7" $ [fmt|a%3dc|]  (7 ∷ Int)      @?= ("a  7c" ∷ 𝕋)
    , testCase "a%3dc:-7" $ [fmt|a%3dc|] (-7 ∷ Int)      @?= ("a -7c" ∷ 𝕋)
    , testCase "a%03dc"   $ [fmt|a%03dc|] (7 ∷ Int)      @?= ("a007c" ∷ 𝕋)
    , testCase "a%-3dc"   $ [fmt|a%-3dc|] (7 ∷ Int)      @?= ("a7  c" ∷ 𝕋)
    , testCase "a%3dc: 7" $ [fmt|a%3nc|]  (7 ∷ Natural)  @?= ("a  7c" ∷ 𝕋)
      -- I'm not sure what -03d should be - it would make sense to me to be
      -- '700', but bash & perl both say '7  '

    , testCase "a%3xc"  $ [fmt|a%3xc|]   (175 ∷ Int)     @?= ("a afc" ∷ 𝕋)
    , testCase "a%-3xc" $ [fmt|a%-3xc|]  (175 ∷ Integer) @?= ("aaf c" ∷ 𝕋)
    , testCase "a%03xc" $ [fmt|a%03xc|]  (175 ∷ Int)     @?= ("a0afc" ∷ 𝕋)
    , testCase "a%,xc"  $ [fmt|a%,xc|] (46175 ∷ Int)     @?= ("ab,45fc" ∷ 𝕋)

    , testCase "a%3oc"   $ [fmt|a%3oc|]   (175 ∷ Int)     @?= ( "a257c"  ∷ 𝕋)
    , testCase "a%-4oc"  $ [fmt|a%-4oc|]  (175 ∷ Integer) @?= ( "a257 c" ∷ 𝕋)
    , testCase "a%04oc"  $ [fmt|a%04oc|]  (175 ∷ Int)     @?= ( "a0257c" ∷ 𝕋)
    , testCase "a%,04oc" $ [fmt|a%,04oc|] (175 ∷ Int)     @?= ( "a0257c" ∷ 𝕋)
    , testCase "a%,05oc" $ [fmt|a%,05oc|] (175 ∷ Int)     @?= ("a0,257c" ∷ 𝕋)

    , testCase "%04b"     $ [fmt|%04b|]      (6 ∷ Int)  @?= (   "0110" ∷ 𝕋)
    , testCase "%-4b"     $ [fmt|%-4b|]      (6 ∷ Int)  @?= (   "110 " ∷ 𝕋)
    , testCase "%2b"      $ [fmt|%2b|]       (6 ∷ Int)  @?= (    "110" ∷ 𝕋)
    , testCase "%,6b"     $ [fmt|%,6b|]     (14 ∷ Int)  @?= ( " 1,110" ∷ 𝕋)

    , testCase "%f 6"       $ [fmtT|%f|]          (6 ∷ Int)    @?= "6"
    , testCase "%f 6.5"     $ [fmtT|%f|]        (6.5 ∷ Float)  @?= "6.5"
    , testCase "%f 6.2"     $ [fmtT|%f|]        (6.2 ∷ Double) @?= "6.2"
    , testCase "%f 1234.5…" $ [fmtT|%f|]  (1234.5678 ∷ Double) @?= "1234.5678"
    , testCase "%f 1234.5…" $ [fmtT|%,f|] (1234.5678 ∷ Double) @?= "1,234.567,8"

    , testCase "%L"       $ [fmtT|(%L)|]     ts  @?=  "(ttt: c,ttt: b,ttt: a)"
    , testCase "%22L"     $ [fmtT|(%22L)|]   ts  @?=  "(  ttt: c,ttt: b,ttt: a)"

    , testCase "%3f"     $ [fmtT|%3f|]    (6 ∷ Int)       @?=  "  6"
    , testCase "%3.2f"   $ [fmtT|%3.2f|]  (6.2 ∷ Float)   @?=  "6.20"
    , testCase "%3.2f"   $ [fmtT|%3.2f|]  (6.005 ∷ Float) @?=  "6.01"
    , testCase "%3.2f"   $ [fmtT|%3.2f|]  (6.002 ∷ Float) @?=  "6.00"
    , testCase "%05.2f"  $ [fmtT|%05.2f|] (6.2 ∷ Double)  @?=  "06.20"
    , testCase "%-5.2f"  $ [fmtT|%-5.2f|] (6.2 ∷ Double)  @?=  "6.20 "
    , testCase "%.3f"    $ [fmtT|%.3f|]   (6.2 ∷ Double)  @?=  "6.200"

    , testCase "%3n"     $ [fmtT|%3n|]    (6 ∷ Int)       @?=  "  6"
    , testCase "%3n"     $ [fmtT|%3n|]    (6 ∷ Word8)     @?=  "  6"
    , testCase "%3n"     $ [fmtT|%3n|]    (6 ∷ Word8)     @?=  "  6"

    , testCase "%e"     $ [fmtT|%e|]       (1000000 ∷ Double)  @?=      "10e5"
    , testCase "%.2e"   $ [fmtT|%.2e|]     (1000000 ∷ Double)  @?=   "10.00e5"
    , testCase "%5.2e"  $ [fmtT|%5.2e|]    (9999999 ∷ Double)  @?=   "10.00e6"
    , testCase "%9.2e"  $ [fmtT|%9.2e|]    (1000000 ∷ Double)  @?= "  10.00e5"
    , testCase "%,9.2e" $ [fmtT|%,9.2e|]   (1000000 ∷ Double)  @?= "  10.00e5"

    , testCase "fmtS"         $ [fmtS|a%03tc|] "b" @?= ("a00bc" ∷ String)
    , testCase "as string"    $ [fmtS|a%03tc|] "b" @?=  "a00bc"
    , testCase "fmtL"       $ [fmtL|a%03tc|] "b" @?= ("a00bc" ∷LT.Text)
    , testCase "as lazy text" $ [fmtL|a%03tc|] "b" @?= "a00bc"
    , testCase "fmtT"        $ [fmtT|a%03tc|] "b" @?= ("a00bc" ∷ 𝕋)
    , testCase "as strict text" $ [fmtT|a%03tc|] "b" @?= "a00bc"

    , testCase "%d"      $ [fmtT|%%%d|] (  7  ∷ Int) @?= ("%7"   ∷ 𝕋)
    , testCase "%d (-)"  $ [fmtT|%d|]   ((-7) ∷ Int) @?= ("-7"  ∷ 𝕋)
    , testCase "%dC"     $ [fmtT|%dC|]  (  7  ∷ Int) @?= ("7C"  ∷ 𝕋)
    , testCase "F%d"     $ [fmtT|F%d|]  (  7  ∷ Int) @?= ("F7"  ∷ 𝕋)
    , testCase "-%d"     $ [fmtT|-%d|]  (  7  ∷ Int) @?= ("-7"  ∷ 𝕋)
    , testCase "-%d"     $ [fmtT|-%d|]  ((-7) ∷ Int) @?= ("--7" ∷ 𝕋)

    , testCase "0 b" $ [fmt|%Y|] (0 ∷ Int) @?= ("0" ∷ 𝕋)
    , testCase "0 B" $ [fmt|%y|] (0 ∷ Int) @?= ("0" ∷ 𝕋)
    , testCase "500 b" $ [fmt|%y|] (500 ∷ Int) @?= ("500B" ∷ 𝕋)
    , testCase "500 B" $ [fmt|%Y|] (500 ∷ Int) @?= ("500B" ∷ 𝕋)
    , testCase "1000 b"  $ [fmt|%y|] (1000 ∷ Int) @?= ("1.00kB" ∷ 𝕋)
    , testCase "1000 B"  $ [fmt|%Y|] (1000 ∷ Int) @?= ("1000B" ∷ 𝕋)
    , testCase "1024 b"  $ [fmt|%y|] (1024 ∷ Int) @?= ("1.02kB" ∷ 𝕋)
    , testCase "1024 B"  $ [fmt|%Y|] (1024 ∷ Int) @?= ("1.00KiB" ∷ 𝕋)
    , testCase "5000 b" $ [fmt|%y|] (5000 ∷ Int) @?= ("5.00kB" ∷ 𝕋)
    , testCase "5000 B" $ [fmt|%Y|] (5000 ∷ Int) @?= ("4.88KiB" ∷ 𝕋)
    , testCase "1000000 b" $ [fmt|%y|] (1000000 ∷ Int) @?= ("1.00MB" ∷ 𝕋)
    , testCase "1000000 B" $ [fmt|%Y|] (1000000 ∷ Int) @?= ("977KiB" ∷ 𝕋)
    , testCase "1048576 b" $ [fmt|%y|] (1048576 ∷ Int) @?= ("1.05MB" ∷ 𝕋)
    , testCase "1048576 B" $ [fmt|%Y|] (1048576 ∷ Int) @?= ("1.00MiB" ∷ 𝕋)
    , testCase "1073741824 b" $ [fmt|%7y|] (1024^^3) @?= (" 1.07GB" ∷ 𝕋)
    , testCase "1073741824 B" $ [fmt|%Y|] (1024^^3) @?= ("1.00GiB" ∷ 𝕋)
    , testCase "1073741824 B" $ [fmt|%Y|] (1024^^3) @?= ("1.00GiB" ∷ 𝕋)

    , testCase "1973-01-01-Z17:00:00" $
          [fmtT|%z|] dayOne @?= "1973-01-01Z17:00:00"
    , testCase "1973-01-01-Z17:00:00 Mon" $
          [fmtT|%Z|] dayOne @?= "1973-01-01Z17:00:00 Mon"

    , testCase "1973-01-01-Z17:00:00" $
          [fmtT|%k|] cs @?= "«foo» (a:b:c#8[13]→21[34])"
    , testCase "1973-01-01-Z17:00:00 Mon" $
          [fmtT|%K|] cs @?= intercalate "\n" [ "«foo» (a:b:c#8[13]→21[34])"
                                             , "«bar» (x:y:z#55[89→233])" ]
    , testCase "1973-01-01-Z17:00:00 Mon" $
          [fmtT|%28K|] cs @?= intercalate "\n" [ "  «foo» (a:b:c#8[13]→21[34])"
                                               , "    «bar» (x:y:z#55[89→233])"]
    , testCase "1973-01-01-Z17:00:00 Mon" $
          [fmtT|%-26K|] cs @?= intercalate "\n" [ "«foo» (a:b:c#8[13]→21[34])"
                                                , "«bar» (x:y:z#55[89→233])  " ]

    -- shell quoting
    , testCase "bob" $ [fmt|%q|] ("bob" ∷ String) @?= ("bob"   ∷ 𝕋)
    , testCase "b b" $ [fmt|%q|] ("b b" ∷ String) @?= ("'b b'" ∷ 𝕋)
    , testCase "b'b" $ [fmt|%q|] ("b'b" ∷ String) @?= ("'b'\\''b'" ∷ 𝕋)

    , testCase "a b" $ [fmt|%Q|] ["a","b" ∷ 𝕊] @?= ("a b" ∷ 𝕋)
    , testCase "a b c" $ [fmt|%Q|] ["a b","c" ∷ 𝕊] @?= ("'a b' c" ∷ 𝕋)

    -- time formatting
    , check           "1s" $ [fmt|%m|]     (    1 ∷ ℕ)
                              -- 1.49 Float is inexact
    , check           "1s" $ [fmt|%m|]     ( 1 + 49%100 ∷ ℚ)
    , check        "1.49s" $ [fmt|%.2m|]   ( 1 + 49%100 ∷ ℚ)
    , check       "1.490s" $ [fmt|%.3m|]   ( 1 + 49%100 ∷ ℚ)
    , check      " 1.490s" $ [fmt|%7.3m|]  ( 1 + 49%100 ∷ ℚ)
    , check         "1.5s" $ [fmt|%.1m|]   ( 1 + 49%100 ∷ ℚ)
    , check           "1s" $ [fmt|%.0m|]   ( 1 + 49%100 ∷ ℚ)
    , check         "  1s" $ [fmt|%4.0m|]  ( 1 + 49%100 ∷ ℚ)
    , check        "1.50s" $ [fmt|%.2m|]   ( 1 + 50%100 ∷ ℚ)
    , check           "2s" $ [fmt|%.0m|]   ( 1 + 50%100 ∷ ℚ)
    , check         "  2s" $ [fmt|%4.0m|]  ( 1 + 50%100 ∷ ℚ)
    , check         "1.4s" $ [fmt|%.1m|]   ( 1 + 449%1000 ∷ ℚ)
    , check         "1.6s" $ [fmt|%.1m|]   ( 1 + 55%100 ∷ ℚ)
    , check         "2.0s" $ [fmt|%.1m|]   ( 1 + 99%100 ∷ ℚ)
    , check        "1.99s" $ [fmt|%.2m|]   ( 1 + 989%1000 ∷ ℚ)
    , check        "2.00s" $ [fmt|%.2m|]   ( 1 + 999%1000 ∷ ℚ)
    , check           "2s" $ [fmt|%.0m|]   ( 1 + 999%1000 ∷ ℚ)
    , check           "2s" $ [fmt|%m|]     (  1.5 ∷ Float)
    , check        "1m02s" $ [fmt|%m|]     ( 61.5 ∷ Float)
    , check     "1h00m02s" $ [fmt|%m|]     (3601.5 ∷ Float)
    , check           "2s" $ [fmt|%.0m|]   (  1.5 ∷ Float)
    , check         "0.667s" $ [fmt|%.3m|] (  2%3 ∷ ℚ)
    , check           "1s" $ [fmt|%m|]     (  2%3 ∷ ℚ)
    , check          " 1s" $ [fmt|%3m|]    (  2%3 ∷ ℚ)
    , check          "01s" $ [fmt|%03m|]   (  2%3 ∷ ℚ)
    , check           "1s" $ [fmt|%.0m|]   (  2%3 ∷ ℚ)
    , check         "0.7s" $ [fmt|%.1m|]   (  2%3 ∷ ℚ)


    , check        "1m01s" $ [fmt|%m|]     (   61 ∷ ℕ)
    , check         "1:01" $ [fmt|%:m|]    (   61 ∷ ℕ)
    , check       "1m01s " $ [fmt|%-6m|]   (   61 ∷ ℕ)
    , check       " 1m01s" $ [fmt|%6m|]    (   61 ∷ ℕ)
    , check       "1:01  " $ [fmt|%:-6m|]  (   61 ∷ ℕ)
    , check       "  1:01" $ [fmt|%:6m|]   (   61 ∷ ℕ)
    , check     "1h01m01s" $ [fmt|%6m|]    ( 3661 ∷ ℕ)
    , check    " 1h01m01s" $ [fmt|%9m|]    ( 3661 ∷ ℕ)
    , check    "1h01m01s " $ [fmt|%-9m|]   ( 3661 ∷ ℕ)
    , check    "-1h01m01s" $ [fmt|%-9m|]   (-3661 ∷ ℤ)
    , check   " -1h01m01s" $ [fmt|%10m|]   (-3661 ∷ ℤ)
    , check   "-1h01m01s " $ [fmt|%-10m|]  (-3661 ∷ ℤ)
    , check    "-1h01m01s" $ [fmt|%-9m|]   (-3661.01 ∷ Float)
    , check  "-1h01m01.0s" $ [fmt|%-9.1m|] (-3661 ∷ ℤ)
    , check  "-1h01m01.0s" $ [fmt|%-9.1m|] (-3661.01 ∷ Float)
    , check " -1h01m01.0s" $ [fmt|%12.1m|] (-3661.01 ∷ Float)
    , check " -1h01m01.1s" $ [fmt|%12.1m|] (-3661.05 ∷ Float)
    , check "-1h01m01.01s" $ [fmt|%12.2m|] (-3661.01 ∷ Float)
    , check "-1h01m01.05s" $ [fmt|%12.2m|] (-3661.05 ∷ Float)
    , check    "-1:01:01 " $ [fmt|%:-9m|]  (-3661 ∷ ℤ)
    , check "-1:01:01 "    $ [fmt|%:-9m|]  (-3661.01 ∷ Float)
    , check "-1:01:01.01 " $ [fmt|%:-12.2m|] (-3661.01 ∷ Float)
    ]

-- that's all, folks! ---------------------------------------------------------
