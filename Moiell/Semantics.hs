{-# LANGUAGE Rank2Types, TypeSynonymInstances #-}
module Moiell.Semantics where

import Moiell.Class
import MonadLibSplit
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map

type Comp       = ReaderT TReader (ExceptionT TException (ChoiceT Id))
type TException = Value
type TReader    = [Object]
type TIdent     = String
type TResult    = [Either TException Value]
type CompMap    = Map.Map TIdent (Comp Value)
type CompValue  = Comp Value

data Value = N Double | S String | A TIdent | O Object
data Object = Ur | Object { parent :: Object, attrs :: CompMap, oEnv :: TReader }

inAttr = "_"
outAttr = "()"


instance Moiell CompValue where

  urObject = return.O $ Ur

  -- object :: Comp Value -> CompMap -> Comp Value -> Comp Value
  object parComp attrs content = do
    val <- parComp
    env <- ask
    case val of
      O par -> return.O $ setAttr outAttr content $ Object par attrs env
      x     -> fail ("Cannot extend from a non-object: " ++ show x)

  attrib = return . A
  string = return . S
  number = return . N
  
  eachC f = mkFun return (f . return)
  eachCS = mkFun toString
  eachCN = mkFun toDouble

  -- apply :: Comp Value -> Comp Value -> Comp Value
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

  csum = msum
  empty = mzero
  split emptyC splitC c = msplit c >>= maybe emptyC (\(h, t) -> splitC (return h) t)
  
  throw = liftC $ (getArg >>= raise) >> mzero
  catch = liftC $ liftC $ (try (inParent getArg) >>= either (apply getArg . return) return)
  fatal = fail

  this = do
    env <- ask
    return.O $ head env
  
  inParent c = do
    env <- ask
    local (tail env) c

  
  -- run :: Comp Value -> String
  run = showResult . runWithEnv globalObject



evalAttr :: TIdent -> Object -> Comp Value
evalAttr attrName obj = local (obj : oEnv obj) (lookupAttr attrName obj)

lookupAttr :: TIdent -> Object -> Comp Value
lookupAttr attrName Ur = fail ("Could not find attribute: " ++ attrName)
lookupAttr attrName obj = Map.findWithDefault (lookupAttr attrName $ parent obj) attrName $ attrs obj

setAttr :: TIdent -> Comp Value -> Object -> Object
setAttr attrName attrValue obj = obj{ attrs = Map.insert attrName attrValue $ attrs obj }


liftC :: Comp Value -> Comp Value
liftC content = do
  env <- ask
  return.O $ setAttr outAttr content $ Object Ur Map.empty env

getArg :: Comp Value
getArg = do
  env <- ask
  evalAttr "_" $ head env

toDouble :: Value -> Comp Double
toDouble (N n) = return n
toDouble (S s) = maybe mzero (return . fst) (listToMaybe (reads s))
toDouble _     = mzero

toString :: Value -> Comp String
toString (S s) = return s
toString v     = return $ show v

mkFun :: (Value -> Comp a) -> (a -> Comp Value) -> Comp Value
mkFun fx f = liftC $ getArg >>= fx >>= f

mkFun2 :: (Value -> Comp a) -> (Value -> Comp b) -> (a -> b -> Comp Value) -> Comp Value
mkFun2 fx fy f = liftC $ do
  x <- fx =<< getArg
  liftC $ do
    y <- fy =<< getArg
    f x y


globalObject :: Object
globalObject = Ur

runWithEnv :: Object -> Comp Value -> [Either TException Value]
runWithEnv env = runId . findAll . runExceptionT . runReaderT [env]

showResult :: [Either TException Value] -> String
showResult = unlines . map (either (("Err: " ++) . show) show)

instance Show Value where
  show (N n) = show n
  show (S s) = show s
  show (O o) = show o
  show (A i) = "{Attribute " ++ i ++ "}" 
  
instance Show Object where
  show Ur = "{}"
  show (Object par prps e) = show par ++ "{" ++ show (Map.keys prps) ++ "}"