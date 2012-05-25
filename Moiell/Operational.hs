{-# LANGUAGE GADTs, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
module Moiell.Operational where

import Moiell.Class
import Moiell.MonadInstance

import Control.Monad.Operational
import Data.Functor.Identity

import Debug.Trace

type PluOp = Program PluI

data PluI a where
  ZeroI  :: PluI a
  PlusI  :: PluOp a -> PluOp a -> PluI a
  SplitI :: PluOp a -> PluI (Maybe (a, PluOp a))
  AskI   :: PluI (TReader PluOp)
  LocalI :: TReader PluOp -> PluOp a -> PluI a
  RaiseI :: TException PluOp -> PluI a
  TryI   :: PluOp a -> PluI (Either (TException PluOp) a)

instance MonadPlus PluOp where
  mzero = singleton ZeroI
  mplus m n = singleton (PlusI m n)

instance RunMonadPlus PluOp where
  msplit m = singleton (SplitI m)

instance ReaderM PluOp (TReader PluOp) where
  ask = singleton AskI

instance RunReaderM PluOp (TReader PluOp) where
  local i m = singleton (LocalI i m)

instance ExceptionM PluOp (TException PluOp) where
  raise e = singleton (RaiseI e)

instance RunExceptionM PluOp (TException PluOp) where
  try m = singleton (TryI m)

type TResult' a = Either (TException PluOp) (Maybe (a, PluOp a))

instance RunWithEnv PluOp where
  runWithEnv globalScope p = case go [globalScope] p of
      Left e -> [Left e]
      Right Nothing -> []
      Right (Just (v, p')) -> Right v : runWithEnv globalScope p'
    where
      go :: TReader PluOp -> PluOp a -> TResult' a
      go env = eval env . view
      
      eval :: TReader PluOp -> ProgramView PluI a -> TResult' a
      eval _   (Return a) = return $ Just (a, mzero)
      eval _   (ZeroI :>>= _) = return Nothing
      eval env (PlusI m n :>>= k) = go env (m >>= k) >>= maybe (go env (n >>= k)) (\(a, as) -> return $ Just (a, as `mplus` (n >>= k)))
      eval env (SplitI m :>>= k) = go env m >>= go env . k
      eval env (AskI :>>= k) = go env (k env)
      eval env (LocalI env' m :>>= k) = go env' m >>= maybe (return Nothing) (\(a, as) -> go env (k a `mplus` (local env' as >>= k)))
      eval _   (RaiseI e :>>= _) = Left e
      eval env (TryI m :>>= k) = either (\e -> go env (k (Left e))) (maybe (return Nothing) (\(a, as) -> go env (k (Right a) `mplus` (try as >>= k)))) (go env m)