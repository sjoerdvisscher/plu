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
data Object = Ur | Object { parent :: Object, attrs :: CompMap, oEnv :: TReader }

inAttr = "_"
outAttr = "()"


urObject :: Comp Value
urObject = return.O $ Ur

object :: Comp Value -> CompMap -> Comp Value -> Comp Value
object parComp attrs content = do
  val <- parComp
  env <- ask
  case val of
    O par -> return.O $ setAttr outAttr content $ Object par attrs env
    x     -> fail ("Cannot extend from a non-object: " ++ show x)

apply :: Comp Value -> Comp Value -> Comp Value
apply fs xs = do
  f <- fs
  case f of
    
    O obj -> do
      env <- ask
      evalAttr outAttr $ setAttr inAttr (local env xs) obj
      
    A attrName -> do
      x <- xs
      case x of
        O obj -> evalAttr attrName obj
        v     -> fail ("Attribute lookup applied to non-object: " ++ show v)
        
    v     -> fail ("Cannot apply a literal value: " ++ show v)


this :: Comp Value
this = do
  env <- ask
  return.O $ head env
  
runInParent :: Comp Value -> Comp Value
runInParent c = do
  env <- ask
  local (tail env) c


evalAttr :: TIdent -> Object -> Comp Value
evalAttr attrName obj = local (obj : oEnv obj) (lookupAttr attrName obj)

lookupAttr :: TIdent -> Object -> Comp Value
lookupAttr attrName Ur = fail ("Could not find attribute: " ++ attrName)
lookupAttr attrName obj = Map.findWithDefault (lookupAttr attrName $ parent obj) attrName $ attrs obj

setAttr :: TIdent -> Comp Value -> Object -> Object
setAttr attrName attrValue obj = obj{ attrs = Map.insert attrName attrValue $ attrs obj }



runWithEnv :: Object -> Comp a -> [Either TException a]
runWithEnv env = runId . findAll . runExceptionT . runReaderT [env]

showResult :: Show a => [Either TException a] -> String
showResult = unlines . map (either ("Err: " ++) show)

instance Show Value where
  show (N n) = show n
  show (S s) = show s
  show (O o) = show o
  show (A i) = "{Attribute " ++ i ++ "}" 
  
instance Show Object where
  show Ur = "{}"
  show (Object par prps e) = show par ++ "{" ++ show (Map.keys prps) ++ "}"
