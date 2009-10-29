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
commaS = liftC $ liftC $ mappend (inParent getArg) getArg
  
eachS :: Moiell c => c
eachS = eachC (\body -> eachC (\arg -> body `apply` arg))

filterS :: Moiell c => c
filterS = eachC (\arg -> eachC (\test -> split mempty (\_ _ -> arg) (test `apply` arg)))
  
notS, andS, orS :: Moiell c => c
notS = liftC $ split unit (\_ _ -> mempty) getArg
andS = liftC $ liftC $ split mempty (\_ _ -> getArg) $ inParent getArg
orS  = liftC $ liftC $ split getArg (\h t -> mappend h t) $ inParent getArg

headS :: Moiell c => c
headS = liftC $ split mempty (\h _ -> h) getArg

tailS :: Moiell c => c
tailS = liftC $ split mempty (\_ t -> t) getArg

eachC :: Moiell c => (c -> c) -> c
eachC f = liftC $ each f getArg

each :: Moiell c => (c -> c) -> c -> c
each f = split mempty (\h t -> mappend (f h) (each f t))