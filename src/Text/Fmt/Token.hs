{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE FlexibleContexts #-}

{- | a token in a format specifier -}

module Text.Fmt.Token
  ( Modifier(..), Token( .. ) )
where

-- base --------------------------------

import Numeric.Natural  ( Natural )

-- text --------------------------------

import Data.Text  ( Text )

--------------------------------------------------------------------------------

-- | a modifier, e.g., prefix ',' to commify numbers

data Modifier = MOD_NONE | MOD_COMMIFY  deriving  (Eq,Show)

------------------------------------------------------------

-- | a token in a format specifier

data Token =
  -- | a conversion specifier, e.g., %03.2{xx}f;
    Conversion Modifier                -- ^ the modifier, e.g., ',' for
                                       --   commification
               (Maybe (Integer, Char)) -- ^ the fill, if any - the width, and
                                       --   the char (default ' ', e.g.,
                                       --   typically '0' for numbers); in our
                                       --   example, this would be (3,'0')
               (Maybe Natural)         -- ^ the precision, if any; in our
                                       --   example, this would be 2
               (Maybe Text)            -- ^ string option; in our example, this
                                       --   would be "xx"
               Char                    -- ^ conversion char; in our example,
                                       --   this would be 'f'
  | Str String
  deriving (Eq, Show)

-- that's all, folks! ----------------------------------------------------------
