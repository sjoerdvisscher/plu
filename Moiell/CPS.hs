{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module Moiell.CPS (CPSComp) where

import Moiell.MonadInstance

import Data.Function (on)


data CPSData a = CPSData 
  { yield  :: a -> TResult CPSComp
  , done   :: TResult CPSComp
  , throwC :: TException CPSComp -> TResult CPSComp
  , choice :: CPSComp a -> CPSComp a -> TResult CPSComp
  , env    :: TReader CPSComp
  }
data CPSComp a = CPS { runCPS :: CPSData a -> TResult CPSComp }


runWith :: CPSData a -> CPSComp a -> CPSComp a
runWith c1 m = CPS $ \c -> runCPS m c{ env = env c1, throwC = throwC c1 }

runChoice :: CPSData b -> (CPSComp a -> CPSComp b) -> CPSComp a -> CPSComp a -> TResult CPSComp
runChoice c1 f = choice c1 `on` (runWith c1 . f)


instance Functor CPSComp where
  fmap  = liftM
  
instance Monad CPSComp where
  return x = CPS $ \c -> yield c x
  (CPS m) >>= f = CPS $ \c -> m $
    c{ yield  = \a -> runCPS (f a) c
     , choice = runChoice c (>>= f) 
     }

instance MonadPlus CPSComp where
  mzero = CPS $ \c -> done c
  mplus l r = CPS $ \c -> runChoice c id l r

instance ReaderM CPSComp (TReader CPSComp) where
  ask = CPS $ \c -> yield c (env c)

instance RunReaderM CPSComp (TReader CPSComp) where
  local i (CPS m) = CPS $ \c -> m c{ env = i }

instance ExceptionM CPSComp (TException CPSComp) where
  raise e = CPS $ \c -> throwC c e

instance RunExceptionM CPSComp (TException CPSComp) where
  try (CPS m) = CPS $ \c -> m $
    c{ yield = \a -> yield c (Right a)
     , throwC = \e -> yield c (Left e)
     , choice = runChoice c try
     }

instance RunMonadPlus CPSComp where
  msplit (CPS m) = CPS $ \c -> 
    let c1 = c{
         yield  = \a -> yield c (Just (a, mzero))
       , done   = yield c Nothing
       , choice = 
         let ch l r = runCPS l $ c{
            yield = \a -> yield c (Just (a, r))
          , done  = runCPS r c1
          , choice = \ll lr -> ch ll (mplus lr r)
          }
         in ch
       }
    in m c1
   
instance RunWithEnv CPSComp where
  runWithEnv globalScope = flip runCPS c where
    c = CPSData 
          { yield  = \a -> [Right a]
          , done   = [] 
          , throwC = \a -> [Left a]
          , choice = \l r -> runCPS l c ++ runCPS r c
          , env    = [globalScope]
          }
