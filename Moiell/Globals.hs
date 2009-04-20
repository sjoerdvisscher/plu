module Moiell.Globals (globalScope, globalObject) 
where

import Moiell.Semantics

import MonadLibSplit
import Control.Applicative
import Data.Maybe
import Data.Fixed (divMod')
import qualified Data.Map as Map

globalScope :: CompMap
globalScope = Map.fromList 
  [ ("_", pure $ A "_")
  , ("unit", unit)
  , ("Each", eachS)
  , ("And", andS)
  , ("Or", orS)
  , ("Not",  notS)
  , ("Head", headS)
  , ("Tail", tailS)
  , ("Filter", filterS)
  , ("+", mkFun2 pure pure plus)
  , ("-", mkBinOp (-))
  , ("*", mkBinOp (*))
  , ("/", mkBinOp (/))
  , ("mod", mkBinOp (\l r -> snd (l `divMod'` r)))
  , ("<", filterN2 (<))
  , ("<=", filterN2 (<=))
  , (">", filterN2 (>))
  , (">=", filterN2 (>=))
  , ("==", filterN2 (==))
  , ("!=", filterN2 (/=))
  ]

globalObject :: Object
globalObject = Ur

liftC :: Comp Value -> Comp Value
liftC = object Map.empty

unit :: Comp Value
unit = liftC unit

getArg :: Comp Value
getArg = apply (pure $ A "_") this

mkFun :: (Comp Value -> Comp a) -> (a -> Comp Value) -> Comp Value
mkFun fx f = liftC $ do
  x <- fx getArg
  f x

mkFun2 :: (Value -> Comp a) -> (Value -> Comp b) -> (a -> b -> Comp Value) -> Comp Value
mkFun2 fx fy f = liftC $ do
  x <- fx =<< getArg
  liftC $ do
    y <- fy =<< getArg
    f x y

mkBinOp :: (Double -> Double -> Double) -> Comp Value
mkBinOp op = mkFun2 toDouble toDouble (\l r -> pure . N $ op l r)

toDouble :: Value -> Comp Double
toDouble (N n) = pure n
toDouble (S s) = maybe empty (pure . fst) (listToMaybe (reads s))
toDouble (C c) = toDouble $ S [c]
toDouble _    = empty

plus :: Value -> Value -> Comp Value
plus (S s) y = pure.S $ s ++ show y
plus (C c) y = pure.S $ c : show y
plus y (S s) = pure.S $ show y ++ s
plus y (C c) = pure.S $ show y ++ [c]
plus (N n1) (N n2) = pure.N $ n1 + n2
plus _ _ = empty
  
eachS :: Comp Value
eachS = mkFun2 pure pure (\body arg -> apply (pure body) (pure arg))

filterS :: Comp Value
filterS = mkFun2 pure pure (\arg test -> ifS (apply (pure test) (pure arg)) (pure arg) empty)

ifS :: Comp Value -> Comp Value -> Comp Value -> Comp Value
ifS testComp trueComp falseComp = msplit testComp >>= maybe falseComp (const trueComp)
  
notS, andS, orS :: Comp Value
notS = liftC $ ifS getArg empty unit
andS = liftC $ liftC $ ifS (runInParent getArg) getArg empty
orS  = liftC $ liftC $ msplit (runInParent getArg) >>= maybe getArg (\(h, t) -> pure h <|> t)

filterN2 :: (Double -> Double -> Bool) -> Comp Value
filterN2 op = mkFun2 toDouble toDouble (\a b -> if (op a b) then (pure $ N a) else empty)

headS :: Comp Value
headS = liftC $ do
  s <- msplit getArg
  maybe empty (pure . fst) s

tailS :: Comp Value
tailS = liftC $ do
  s <- msplit getArg
  maybe empty snd s