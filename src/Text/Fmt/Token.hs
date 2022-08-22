{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE FlexibleContexts #-}

{- | a token in a format specifier -}

module Text.Fmt.Token
  ( Modifier(..), Token( .. ) )
where

import Base0T

-- more-unicode ------------------------

import Data.MoreUnicode.Char   ( ℂ )
import Data.MoreUnicode.Maybe  ( 𝕄 )
import Data.MoreUnicode.Text   ( 𝕋 )

--------------------------------------------------------------------------------

-- | a modifier, e.g., prefix ',' to commify numbers

data Modifier = MOD_NONE | MOD_COMMIFY  deriving  (Eq,Show)

------------------------------------------------------------

-- | a token in a format specifier

data Token = Str String
           | -- | a conversion specifier, e.g., %03.2{xx}f;
             Conversion Modifier    -- ^ the modifier, e.g., ',' for
                                    --   commification
                        (𝕄 (ℤ, ℂ)) -- ^ the fill, if any - the width, and
                                    --   the char (default ' ', e.g.,
                                    --   typically '0' for numbers); in our
                                    --   example, this would be (3,'0')
                        (𝕄 ℕ)      -- ^ the precision, if any; in our
                                    --   example, this would be 2
                        (𝕄 𝕋)      -- ^ string option; in our example, this
                                    --   would be "xx"
                        ℂ           -- ^ conversion char; in our example,
                                    --   this would be 'f'
  deriving (Eq, Show)

-- that's all, folks! ----------------------------------------------------------
