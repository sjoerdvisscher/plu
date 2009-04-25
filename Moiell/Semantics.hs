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
  return.O $ Object env p c

apply :: Comp Value -> Comp Value -> Comp Value
apply ops args = do
  val <- ops
  case val of
    O obj -> do
      eval $ setAttr "_" args obj
    A idt -> do
      arg <- args
      case arg of
        O obj -> lookupAttr idt obj
        x     -> fail ("Attribute lookup applied to non-object: " ++ show x)
    x     -> fail ("Cannot apply a literal value: " ++ show x)

eval :: Comp Value -> Comp Value
eval c = do
  val <- c
  case val of
    O obj -> force obj
    x     -> fail ("Cannot evaluate a non-object: " ++ show x)

force :: Object -> Comp Value
force obj = withThis obj (contents obj)

this :: Comp Value
this = do
  env <- getThis
  return.O $ env
  
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

setAttr :: TIdent -> Comp Value -> Object -> Comp Value
setAttr i args obj = do
  env <- getThis
  return.O $ obj{ props = Map.insert i (thunk args env) $ props obj }

thunk :: Comp Value -> Object -> Comp Value
thunk c obj = force $ obj{ contents = c }


run :: Object -> Comp a -> [(Either TException a, TWriter)]
run env = runId . findAll . runWriterT . runExceptionT . runReaderT env

instance Show Value where
  show (N n) = show n
  show (S s) = s
  show (C c) = [c]
  show (O _) = "{Object}"
  show (A i) = "{Attribute " ++ i ++ "}" 