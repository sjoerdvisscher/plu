module Moiell.Globals (globalScope, globalObject) 
where

import Moiell.Semantics

import MonadLibSplit
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map

globalScope :: CompMap
globalScope = Map.fromList 
  [ ("_", return $ A "_")
  , ("unit", unit)
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
mkFun fx f = liftC $ do
  x <- fx =<< getArg
  f x

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
toDouble (C c) = toDouble $ S [c]
toDouble _     = mzero

toString :: Value -> Comp String
toString = return . show
  
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
headS = liftC $ do
  s <- msplit getArg
  maybe mzero (return . fst) s

tailS :: Comp Value
tailS = liftC $ do
  s <- msplit getArg
  maybe mzero snd s
  
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