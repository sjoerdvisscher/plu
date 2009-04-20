module Moiell.Semantics where

import MonadLibSplit
import qualified Data.Map as Map

type TException = String
type TWriter    = [String]
type TReader    = Object
type TIdent     = String

type Comp = ReaderT TReader (ExceptionT TException (WriterT TWriter (ChoiceT Id)))
type CompMap = Map.Map TIdent (Comp Value)

data Value = N Double | S String | C Char | A TIdent | O Object
data Object = Ur | Object { parent :: Object, props :: CompMap, contents :: Comp Value }

object :: CompMap -> Comp Value -> Comp Value
object p c = do
  env <- getThis
  return $ O $ Object env p c

apply :: Comp Value -> Comp Value -> Comp Value
apply ops args = do
  val <- ops
  case val of
    O obj -> do
      env <- getThis
      withThis obj{ props = Map.insert "_" (thunk args env) $ props obj } (contents obj)
    A idt -> do
      arg <- args
      case arg of
        O obj -> force $ lookupAttr idt obj
        x     -> fail ("Attribute lookup applied to non-object: " ++ show x)
    x     -> fail ("Cannot apply a literal value: " ++ show x)

thunk :: Comp Value -> Object -> Comp Value
thunk c o = return $ O o { contents = c }

force :: Comp Value -> Comp Value
force c = do
  val <- c
  case val of
    O obj -> withThis obj (contents obj)
    x     -> fail ("Cannot force a non-object: " ++ show x)

this :: Comp Value
this = do
  env <- getThis
  return $ O env
  
getThis :: Comp Object
getThis = ask

withThis :: Object -> Comp Value -> Comp Value
withThis = local

runInParent :: Comp Value -> Comp Value
runInParent c = do
  env <- getThis
  withThis (parent env) c

lookupAttr :: TIdent -> Object -> Comp Value
lookupAttr i Ur = fail ("Could not find attribute: " ++ i)
lookupAttr i obj = Map.findWithDefault (lookupAttr i $ parent obj) i $ props obj

run :: Object -> Comp a -> [(Either TException a, TWriter)]
run this = runId . findAll . runWriterT . runExceptionT . runReaderT this

instance Show Value where
  show (N n) = show n
  show (S s) = s
  show (C c) = [c]
  show (O o) = "{Object}"
