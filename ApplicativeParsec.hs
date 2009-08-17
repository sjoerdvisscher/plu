{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module ApplicativeParsec
    (
      module Control.Applicative
    , module Text.Parsec
    , module Control.Monad.Error
    ) where

import Control.Applicative
-- Hide a few names that are provided by Applicative.
import Text.Parsec hiding (many, optional, (<|>))
import Control.Monad.Error
import qualified Data.Text as T

instance (Monad m) => Stream T.Text m Char where
    uncons = return . T.uncons
    
instance Error ParseError