module Moiell.Globals (globalScope) 
where

import Moiell.Class

import qualified Data.Map as Map

globalScope :: Moiell c => Map.Map String c
globalScope = Map.fromList 
  [ ("_", attrib "_")
  , (",", commaS)
  , ("unit", unit)
  , ("Attr", liftCS attrib)
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
  , ("++", liftC2S (\l r -> string $ l ++ r))
  , ("<", filterN2 (<))
  , ("<=", filterN2 (<=))
  , (">", filterN2 (>))
  , (">=", filterN2 (>=))
  , ("==", filterN2 (==))
  , ("!=", filterN2 (/=))
  , ("chars", charsS)
  , ("throw", throw)
  , ("catch", Moiell.Class.catch)
  ]

liftC :: Moiell c => c -> c
liftC = object urObject Map.empty

unit :: Moiell c => c
unit = liftC unit

getArg :: Moiell c => c
getArg = apply (attrib "_") this

mkBinOp :: Moiell c => (Double -> Double -> Double) -> c
mkBinOp op = liftC2N (\l r -> number $ op l r)

commaS :: Moiell c => c
commaS = liftC $ liftC $ csum [inParent getArg, getArg]
  
eachS :: Moiell c => c
eachS = eachC2 apply

filterS :: Moiell c => c
filterS = eachC2 (\arg test -> split empty (\h t -> arg) (test `apply` arg))
  
notS, andS, orS :: Moiell c => c
notS = liftC $ split unit (\h t -> empty) getArg
andS = liftC $ liftC $ split empty (\h t -> getArg) $ inParent getArg
orS  = liftC $ liftC $ split getArg (\h t -> csum [h, t]) $ inParent getArg

filterN2 :: Moiell c => (Double -> Double -> Bool) -> c
filterN2 op = liftC2N (\a b -> if op a b then number a else empty)

headS :: Moiell c => c
headS = liftC $ split empty (\h t -> h) getArg

tailS :: Moiell c => c
tailS = liftC $ split empty (\h t -> t) getArg
  
charsS :: Moiell c => c
charsS = liftCS $ csum . map (string . (:[]))


divMod' :: Double -> Double -> (Double, Double)
divMod' n d = (f, n - f * d) where f = fromIntegral $ floor $ n / d