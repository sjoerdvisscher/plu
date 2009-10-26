{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module Moiell.CPS (CPS) where

import Moiell.MonadInstance

import Data.Function (on)


data CPSData a = CPSData 
  { done   :: TResult CPS
  , yield  :: a -> TResult CPS
  , choice :: CPS a -> CPS a -> TResult CPS
  , throwC :: TException CPS -> TResult CPS
  , env    :: TReader CPS
  }
data CPS a = CPS { unCPS :: CPSData a -> TResult CPS }

runCPS :: CPSData a -> CPS a -> TResult CPS
runCPS = flip unCPS

runWith :: CPSData a -> CPS a -> CPS a
runWith c1 m = CPS $ \c -> unCPS m c{ env = env c1, throwC = throwC c1 }

runChoice :: CPSData b -> (CPS a -> CPS b) -> CPS a -> CPS a -> TResult CPS
runChoice c1 f = choice c1 `on` (runWith c1 . f)


instance Functor CPS where
  fmap = liftM
  
instance Monad CPS where
  return x  = CPS $ \c -> yield c x
  m >>= f   = CPS $ \c -> unCPS m c{ yield  = runCPS c . f, choice = runChoice c (>>= f) }

instance MonadPlus CPS where
  mzero     = CPS $ \c -> done c
  mplus l r = CPS $ \c -> runChoice c id l r

instance ReaderM CPS (TReader CPS) where
  ask       = CPS $ \c -> yield c (env c)

instance RunReaderM CPS (TReader CPS) where
  local i m = CPS $ \c -> unCPS m c{ env = i, choice = runChoice c (local i) }

instance ExceptionM CPS (TException CPS) where
  raise e   = CPS $ \c -> throwC c e

instance RunExceptionM CPS (TException CPS) where
  try m     = CPS $ \c -> unCPS m c{ yield  = yield c . Right, throwC = yield c . Left, choice = runChoice c try }

instance RunMonadPlus CPS where
  msplit m = CPS $ \c -> let
      ms = runCPS c{ done = yield c Nothing, yield = conc mzero, choice = ch }
      ch l r = unCPS l c{ done = ms r, yield = conc r, choice = \ll lr -> ch ll (mplus lr r) }
      conc as a = yield c $ Just (a, as)
    in ms m
   
instance RunWithEnv CPS where
  runWithEnv globalScope = r where 
    r = runCPS c
    c = CPSData { done = [], yield = return . Right, choice = (++) `on` r, throwC = return . Left, env = [globalScope] }
