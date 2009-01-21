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
import qualified Data.String.UTF8 as U

instance (Monad m, U.UTF8Bytes string index) => Stream (U.UTF8 string) m Char where
    uncons = return . U.uncons
    
instance Error ParseError