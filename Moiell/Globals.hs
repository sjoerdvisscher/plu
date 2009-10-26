module Moiell.Globals (globalScope) 
where

import Moiell.Class

import qualified Data.Map as Map
import Data.Monoid

globalScope :: Moiell c => Map.Map String c
globalScope = mappend builtIns $ Map.fromList 
  [ ("_", attrib "_")
  , (",", commaS)
  , ("unit", unit)
  , ("Each", eachS)
  , ("And", andS)
  , ("Or", orS)
  , ("Not",  notS)
  , ("Head", headS)
  , ("Tail", tailS)
  , ("Filter", filterS)
  , ("throw", liftC $ throw getArg)
  , ("catch", liftC $ liftC $ (inParent getArg) `Moiell.Class.catch` getArg)
  ]

liftC :: Moiell c => c -> c
liftC = object urObject Map.empty Map.empty

unit :: Moiell c => c
unit = liftC unit

getArg :: Moiell c => c
getArg = apply (attrib "_") this

commaS :: Moiell c => c
commaS = liftC $ liftC $ csum [inParent getArg, getArg]
  
eachS :: Moiell c => c
eachS = eachC (\body -> eachC (\arg -> body `apply` arg))

filterS :: Moiell c => c
filterS = eachC (\arg -> eachC (\test -> split empty (\_ _ -> arg) (test `apply` arg)))
  
notS, andS, orS :: Moiell c => c
notS = liftC $ split unit (\_ _ -> empty) getArg
andS = liftC $ liftC $ split empty (\_ _ -> getArg) $ inParent getArg
orS  = liftC $ liftC $ split getArg (\h t -> csum [h, t]) $ inParent getArg

headS :: Moiell c => c
headS = liftC $ split empty (\h _ -> h) getArg

tailS :: Moiell c => c
tailS = liftC $ split empty (\_ t -> t) getArg

eachC :: Moiell c => (c -> c) -> c
eachC f = liftC $ each f getArg

each :: Moiell c => (c -> c) -> c -> c
each f = split empty (\h t -> csum [f h, each f t])