{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE DeriveLift       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax    #-}

{- | a token in a format specifier -}

module Text.Fmt.Token
  ( Modifier(..)
  , Token(..)
  ) where

import Base0T

-- more-unicode ------------------------

import Data.MoreUnicode.Char  ( ℂ )
import Data.MoreUnicode.Maybe ( 𝕄 )
import Data.MoreUnicode.Text  ( 𝕋 )

-- template-haskell --------------------

import Language.Haskell.TH.Syntax ( Lift )

--------------------------------------------------------------------------------

-- | a modifier, e.g., prefix ',' to commify numbers

data Modifier = MOD_NONE | MOD_COMMIFY | MOD_COLON deriving (Eq, Lift, Show)

------------------------------------------------------------

-- | a token in a format specifier

data Token = Str String
           -- | a conversion specifier, e.g., %03.2{xx}f;
           | Conversion Modifier (𝕄 (ℤ, ℂ)) (𝕄 ℕ) (𝕄 𝕋) ℂ
           -- ^ conversion char; in our example,
           --   this would be 'f'
  deriving (Eq, Show)

-- that's all, folks! ----------------------------------------------------------
