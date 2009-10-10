{-# LANGUAGE Rank2Types #-}
module Moiell.Semantics where

import MonadLibSplit
import qualified Data.Map as Map

type TException = String
type TReader    = [Object]
type TIdent     = String
type TResult    = [Either TException Value]
type Comp       = ReaderT TReader (ExceptionT TException (ChoiceT Id))
type CompMap    = Map.Map TIdent (Comp Value)

data Value = N Double | S String | A TIdent | O Object
data Object = Ur | Object { parent :: Object, props :: CompMap, contents :: Comp Value, oEnv :: TReader }

urObject :: Comp Value
urObject = return.O $ Ur

object :: Comp Value -> CompMap -> Comp Value -> Comp Value
object parComp ps c = do
  val <- parComp
  env <- ask
  case val of
    O par -> return.O $ Object par ps c env
    x     -> fail ("Cannot extend from a non-object: " ++ show x)

apply :: Comp Value -> Comp Value -> Comp Value
apply ops args = wrapDebug "apply outer" $ do
  val <- ops
  case val of
    O obj -> wrapDebug "apply object" $ do
      newObj <- setAttr "_" args obj
      local (newObj : oEnv newObj) (contents newObj)
    A idt -> wrapDebug "apply attr" $ do
      -- put ["apply attr"]
      arg <- args
      case arg of
        O obj -> withThis obj $ lookupAttr idt obj
        x     -> fail ("Attribute lookup applied to non-object: " ++ show x)
    x     -> fail ("Cannot apply a literal value: " ++ show x)

eval :: Comp Value -> Comp Value
eval c = do
  val <- c
  case val of
    O obj -> contents obj
    x     -> fail ("Cannot evaluate a non-object: " ++ show x)


this :: Comp Value
this = do
  env <- getThis
  return.O $ env
  
getThis :: Comp Object
getThis = do
  these <- ask
  return $ head these

withThis :: Object -> Comp Value -> Comp Value
withThis o c = do
  these <- ask
  local (o:these) (wrapDebug "withThis inner" c)

runInParent :: Comp Value -> Comp Value
runInParent c = do
  these <- ask
  local (tail these) (wrapDebug "runInParent inner" c)

debugStack s = do
  these <- ask
  raise $ s ++ show these
  mzero

wrapDebug :: String -> Comp a -> Comp a
-- wrapDebug s c = (debugStack ("Before " ++ s)) `mplus` c `mplus` (debugStack ("After " ++ s))
wrapDebug s c = c

lookupAttr :: TIdent -> Object -> Comp Value
lookupAttr i Ur = fail ("Could not find attribute: " ++ i)
lookupAttr i obj = Map.findWithDefault (lookupAttr i $ parent obj) i $ props obj

setAttr :: TIdent -> Comp Value -> Object -> Comp Object
setAttr i args obj = wrapDebug "outer setAttr" $ do
  these <- ask
  return $ obj{ props = Map.insert i (wrapDebug "middle setAttr" $ local these (wrapDebug "inner setAttr" args)) $ props obj }



runWithEnv :: Object -> Comp a -> [Either TException a]
runWithEnv env = runId . findAll . runExceptionT . runReaderT [env]

showResult :: Show a => [Either TException a] -> String
showResult = unlines . map (either ("Err: " ++) show)

instance Show Value where
  show (N n) = show n
  show (S s) = s
  show (O o) = show o
  show (A i) = "{Attribute " ++ i ++ "}" 
  
instance Show Object where
  show Ur = "{}"
  show (Object par prps c e) = show par ++ "{" ++ show (Map.keys prps) ++ "}"
