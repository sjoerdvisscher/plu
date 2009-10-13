{-# LANGUAGE Rank2Types, MultiParamTypeClasses, TypeSynonymInstances #-}
module Moiell.Semantics where

import Moiell
import MonadLibSplit
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Foldable (foldMap)

type Comp       = ReaderT TReader (ExceptionT TException (ChoiceT Id))
type TException = String
type TReader    = [Object]
type TIdent     = String
type TResult    = [Either TException Value]
type CompMap    = Map.Map TIdent (Comp Value)

data Value = N Double | S String | A TIdent | O Object
data Object = Ur | Object { parent :: Object, attrs :: CompMap, oEnv :: TReader }

inAttr = "_"
outAttr = "()"


instance Moiell Comp Value where

  -- urObject :: Comp Value
  urObject = return.O $ Ur

  -- object :: Comp Value -> CompMap -> Comp Value -> Comp Value
  object parComp attrs content = do
    val <- parComp
    env <- ask
    case val of
      O par -> return.O $ setAttr outAttr content $ Object par attrs env
      x     -> fail ("Cannot extend from a non-object: " ++ show x)

  -- string :: String -> Comp Value
  string = return . S
  
  -- number :: Double -> Comp Value
  number = return . N

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


  -- this :: Comp Value
  this = do
    env <- ask
    return.O $ head env
  
  -- runInParent :: Comp Value -> Comp Value
  runInParent c = do
    env <- ask
    local (tail env) c

  
  -- globalScope :: CompMap
  globalScope = globalScopeS

  -- run :: Comp Value -> String
  run = showResult . runWithEnv globalObject



evalAttr :: TIdent -> Object -> Comp Value
evalAttr attrName obj = local (obj : oEnv obj) (lookupAttr attrName obj)

lookupAttr :: TIdent -> Object -> Comp Value
lookupAttr attrName Ur = fail ("Could not find attribute: " ++ attrName)
lookupAttr attrName obj = Map.findWithDefault (lookupAttr attrName $ parent obj) attrName $ attrs obj

setAttr :: TIdent -> Comp Value -> Object -> Object
setAttr attrName attrValue obj = obj{ attrs = Map.insert attrName attrValue $ attrs obj }



runWithEnv :: Object -> Comp Value -> [Either TException Value]
runWithEnv env = runId . findAll . runExceptionT . runReaderT [env]

showResult :: [Either TException Value] -> String
showResult = unlines . map (either ("Err: " ++) show)

instance Show Value where
  show (N n) = show n
  show (S s) = show s
  show (O o) = show o
  show (A i) = "{Attribute " ++ i ++ "}" 
  
instance Show Object where
  show Ur = "{}"
  show (Object par prps e) = show par ++ "{" ++ show (Map.keys prps) ++ "}"




globalScopeS :: CompMap
globalScopeS = Map.fromList 
  [ ("_", return $ A "_")
  , (",", commaS)
  , ("unit", unit)
  , ("Attr", mkFun toString (return.A))
  , ("Each", eachS)
  , ("And", andS)
  , ("Or", orS)
  , ("Not",  notS)
  , ("Head", headS)
  , ("Tail", tailS)
  , ("Filter", filterS)
  , ("+", mkBinOp (+))
  , ("-", mkBinOp (-))
  , ("*", mkBinOp (*))
  , ("/", mkBinOp (/))
  , ("div", mkBinOp (\l r -> fst (l `divMod'` r)))
  , ("mod", mkBinOp (\l r -> snd (l `divMod'` r)))
  , ("++", mkFun2 toString toString (\l r -> return . S $ l ++ r))
  , ("<", filterN2 (<))
  , ("<=", filterN2 (<=))
  , (">", filterN2 (>))
  , (">=", filterN2 (>=))
  , ("==", filterN2 (==))
  , ("!=", filterN2 (/=))
  , ("chars", charsS)
  , ("throw", mkFun return throwS)
  , ("catch", catchS)
  ]

globalObject :: Object
globalObject = Ur

liftC :: Comp Value -> Comp Value
liftC = object (return.O $ Ur) Map.empty

unit :: Comp Value
unit = liftC unit

getArg :: Comp Value
getArg = apply (return $ A "_") this

mkFun :: (Value -> Comp a) -> (a -> Comp Value) -> Comp Value
mkFun fx f = liftC $ getArg >>= fx >>= f

mkFun2 :: (Value -> Comp a) -> (Value -> Comp b) -> (a -> b -> Comp Value) -> Comp Value
mkFun2 fx fy f = liftC $ do
  x <- fx =<< getArg
  liftC $ do
    y <- fy =<< getArg
    f x y

mkBinOp :: (Double -> Double -> Double) -> Comp Value
mkBinOp op = mkFun2 toDouble toDouble (\l r -> return . N $ op l r)

toDouble :: Value -> Comp Double
toDouble (N n) = return n
toDouble (S s) = maybe mzero (return . fst) (listToMaybe (reads s))
toDouble _     = mzero

toString :: Value -> Comp String
toString (S s) = return s
toString v     = return $ show v

commaS :: Comp Value
commaS = liftC $ liftC $ mplus (runInParent getArg) getArg
  
eachS :: Comp Value
eachS = mkFun2 return return (\body arg -> apply (return body) (return arg))

filterS :: Comp Value
filterS = mkFun2 return return (\arg test -> ifS (apply (return test) (return arg)) (const $ return arg) mzero)

ifS :: Comp Value -> ((Value, Comp Value) -> Comp Value) -> Comp Value -> Comp Value
ifS testComp th falseComp = msplit testComp >>= maybe falseComp th
  
notS, andS, orS :: Comp Value
notS = liftC $ ifS getArg (const mzero) unit
andS = liftC $ do x <- msplit getArg; liftC $ maybe mzero (const getArg) x
orS  = liftC $ do x <- msplit getArg; liftC $ maybe getArg (\(h, t) -> return h `mplus` t) x

filterN2 :: (Double -> Double -> Bool) -> Comp Value
filterN2 op = mkFun2 toDouble toDouble (\a b -> if (op a b) then (return $ N a) else mzero)

headS :: Comp Value
headS = liftC $ msplit getArg >>= maybe mzero (return . fst)

tailS :: Comp Value
tailS = liftC $ msplit getArg >>= maybe mzero snd
  
charsS :: Comp Value
charsS = liftC $ getArg >>= toString >>= foldMap (return.S.(:[]))

throwS :: Value -> Comp Value
throwS a = do
  raise (show a)
  mzero
  
catchS :: Comp Value
catchS = liftC $ liftC $ (try c >>= either (apply hnd . return . S) return)
  where
    c = runInParent getArg
    hnd = getArg

divMod' :: Double -> Double -> (Double, Double)
divMod' n d = (f, n - f * d) where f = fromIntegral $ floor $ n / d