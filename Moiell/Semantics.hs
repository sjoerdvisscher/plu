{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Moiell.Semantics (Comp) where

import Moiell.MonadInstance

newtype Comp a = C { unC :: ReaderT (TReader Comp) (ExceptionT (TException Comp) (ChoiceT Id)) a }
  deriving (Monad, MonadPlus, RunMonadPlus)

instance RunWithEnv Comp where
  runWithEnv env = runId . findAll . runExceptionT . runReaderT [env] . unC
  
instance ReaderM Comp (TReader Comp) where
  ask = C ask
instance RunReaderM Comp (TReader Comp) where
  local i (C m) = C (local i m)

instance ExceptionM Comp (TException Comp) where
  raise e = C (raise e)
instance RunExceptionM Comp (TException Comp) where
  try (C m) = C (try m)