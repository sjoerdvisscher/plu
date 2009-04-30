{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Moiell.DebugSemantics where

import Control.Monad
import qualified Data.Map as Map
import Data.List (intersperse)

newtype Comp a = Comp [a] deriving (Monad, MonadPlus)

type TException = String
type TReader    = [Object]
type TIdent     = String
type TResult    = [String]
type CompMap    = Map.Map TIdent (Comp Value)

data Value = N Double | S String | C Char | A TIdent | O Object
data Object = Ur | Object { parent :: Object, props :: CompMap, contents :: Comp Value }

globalScope :: CompMap
globalScope = Map.fromList (map (\x -> (x, return.S $ x))
  [ "_", "!=", "+", "-", "<", ">=", "And", "Each", "Filter", "Or", "Tail", "mod"])

globalObject :: Object
globalObject = Ur

this :: Comp Value
this = return.S $ "$"

urObject :: Comp Value
urObject = return.S $ ""

object :: Comp Value -> CompMap -> Comp Value -> Comp Value
object par _ c = do
  return.S $ show par ++ "{" ++ show c ++ "}"

apply :: Comp Value -> Comp Value -> Comp Value
apply ops args = do
  return.S $ show ops ++ "(" ++ show args ++ ")"
  
runInParent :: Comp Value -> Comp Value
runInParent c = return.S $ "^(" ++ show c ++ ")"

runWithEnv :: Object -> Comp Value -> TResult
runWithEnv _ (Comp s) = map show s

showResult :: TResult -> String
showResult = unlines

instance (Show a) => Show (Comp a) where
  show (Comp [x]) = show x
  show (Comp xs) = "(" ++ concat (intersperse ", " $ map show xs) ++ ")"

instance Show Value where
  show (N n) = show n
  show (S s) = s
  show (C c) = [c]
  show (O o) = show o
  show (A i) = "{Attribute " ++ i ++ "}" 
  
instance Show Object where
  show Ur = "{}"
  show (Object par prps c) = show par ++ "{" ++ show (Map.keys prps) ++ "}"