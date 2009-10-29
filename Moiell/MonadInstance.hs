{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Moiell.MonadInstance (
    module Moiell.Class
  , module MonadLibSplit
  
  , M
  , TException
  , TReader
  , TResult
  
  , RunWithEnv(..)
  )
where

import Moiell.Class
import MonadLibSplit
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map

type M m = m (Value m)

type TIdent       = String
type TException m = Value m
type TReader    m = Object m
type TResult    m = [Either (TException m) (Value m)]
type CompMap    m = Map.Map TIdent (M m)

data Value m = N Double | S String | A TIdent | O (Object m)
data Object m = Ur | Object { parent :: Object m, attrs :: CompMap m, oEnv :: TReader m }

inAttr, outAttr :: String
inAttr  = "_"
outAttr = "()"

class    (RunMonadPlus m, RunReaderM m (TReader m), RunExceptionM m (Value m), RunWithEnv m) => MoiellMonad m
instance (RunMonadPlus m, RunReaderM m (TReader m), RunExceptionM m (Value m), RunWithEnv m) => MoiellMonad m
  
instance MoiellMonad m => Moiell (M m) where
  
  urObject = return.O $ Ur

  -- object :: M m -> CompMap -> CompMap -> M m -> M m
  object parComp attrsMap _ content = do
    val <- parComp
    env <- ask
    case val of
      O par -> return.O $ setAttr outAttr content $ Object par attrsMap env
      x     -> fail ("Cannot extend from a non-object: " ++ show x)

  attrib = return . A
  string = return . S
  number = return . N
  
  builtIns = Map.fromList 
    [ ("+", mkBinOp (+))
    , ("-", mkBinOp (-))
    , ("*", mkBinOp (*))
    , ("/", mkBinOp (/))
    , ("~", mkFun toDouble (\x -> number $ -x))
    , ("div", mkBinOp (\l r -> fst (l `divMod'` r)))
    , ("mod", mkBinOp (\l r -> snd (l `divMod'` r)))
    , ("++", mkFun toString (\l -> mkFun toString (\r -> string $ l ++ r)))
    , ("<", filterN2 (<))
    , ("<=", filterN2 (<=))
    , (">", filterN2 (>))
    , (">=", filterN2 (>=))
    , ("==", filterN2 (==))
    , ("!=", filterN2 (/=))
    , ("chars", charsS)
    , ("Attr", mkFun toString attrib)
    ]

  -- apply :: M m -> M m -> M m
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

  -- split :: M m -> (M m -> M m -> M m) -> M m -> M m
  split emptyC splitC c = msplit c >>= maybe emptyC (\(h, t) -> splitC (return h) t)
  
  throw e = (e >>= raise) >> mzero
  c `catch` h = try c >>= either (apply h . return) return
  fatal = fail

  this = do
    env <- ask
    return.O $ env
  
  inParent c = do
    env <- ask
    local (oEnv env) c

  
  -- run :: M m -> String
  run = showResult . runWithEnv globalObject

instance MoiellMonad m => Monoid (M m) where
  mempty = mzero
  mappend = mplus
  
class RunWithEnv m where
  runWithEnv :: Object m -> M m -> TResult m


evalAttr :: MoiellMonad m => TIdent -> Object m -> M m
evalAttr attrName obj = local obj (lookupAttr attrName obj)

lookupAttr :: MoiellMonad m => TIdent -> Object m -> M m
lookupAttr attrName Ur = fail ("Could not find attribute: " ++ attrName)
lookupAttr attrName obj = Map.findWithDefault (lookupAttr attrName $ parent obj) attrName $ attrs obj

setAttr :: MoiellMonad m => TIdent -> M m -> Object m -> Object m
setAttr attrName attrValue obj = obj{ attrs = Map.insert attrName attrValue $ attrs obj }


toDouble :: MoiellMonad m => Value m -> m Double
toDouble (N n) = return n
toDouble (S s) = maybe mzero (return . fst) (listToMaybe (reads s))
toDouble _     = mzero

toString :: MoiellMonad m => Value m -> m String
toString (S s) = return s
toString v     = return $ show v

mkFun :: MoiellMonad m => (Value m -> m a) -> (a -> M m) -> M m
mkFun fx f = object urObject Map.empty Map.empty $ this >>= (\(O o) -> evalAttr "_" o) >>= fx >>= f

eachC :: MoiellMonad m => (M m -> M m) -> M m
eachC f = mkFun return (f . return)

mkBinOp :: MoiellMonad m => (Double -> Double -> Double) -> M m
mkBinOp op = mkFun toDouble (\l -> mkFun toDouble (\r -> number $ op l r))

filterN2 :: MoiellMonad m => (Double -> Double -> Bool) -> M m
filterN2 op = mkFun toDouble (\a -> mkFun toDouble (\b -> if op a b then number a else mzero))

charsS :: MoiellMonad m => M m
charsS = mkFun toString $ foldMap (string . (:[]))

divMod' :: Double -> Double -> (Double, Double)
divMod' n d = (f, n - f * d) where f = fromIntegral $ floor $ n / d


globalObject :: Object m
globalObject = Ur

showResult :: TResult m -> String
showResult = unlines . map (either (("Err: " ++) . show) show)

instance Eq (Value m) where
  (N a) == (N b) = a == b
  (S a) == (S b) = a == b
  (A a) == (A b) = a == b
  _ == _ = False

instance Ord (Value m) where
  (N a) `compare` (N b) = a `compare` b
  (S a) `compare` (S b) = a `compare` b
  (A a) `compare` (A b) = a `compare` b
  _ `compare` _ = error "Cannot compare"
  
instance Show (Value m) where
  show (N n) = show n
  show (S s) = show s
  show (O o) = show o
  show (A i) = "{Attribute " ++ i ++ "}" 
  
instance Show (Object m) where
  show Ur = "{}"
  show (Object par prps _) = show par ++ "{" ++ show (Map.keys prps) ++ "}"