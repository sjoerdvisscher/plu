{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Moiell.CPS (CPS) where

import Moiell.Class

import MonadLibSplit
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map


data CPSData a = CPSData 
  { yield  :: a -> TResult
  , done   :: TResult
  , throwC :: TException -> TResult
  , choice :: CPSComp a -> CPSComp a -> TResult
  }
data CPSComp a = CPS { runCPS :: CPSData a -> TResult }
type Comp = ReaderT TReader CPSComp

type CPS        = Comp Value
type TException = Value
type TReader    = [Object]
type TIdent     = String
type TResult    = [Either TException Value]
type CompMap    = Map.Map TIdent CPS

data Value = N Double | S String | A TIdent | O Object
data Object = Ur | Object { parent :: Object, attrs :: CompMap, oEnv :: TReader }

inAttr = "_"
outAttr = "()"


instance Functor CPSComp where
  fmap  = liftM
  
instance Monad CPSComp where
  return x = CPS $ \c -> yield c x
  (CPS m) >>= f = CPS $ \c -> m $
    c{ yield  = \a -> runCPS (f a) c 
     , choice = \l r -> choice c (l >>= f) (r >>= f)
     }

instance MonadPlus CPSComp where
  mzero = CPS $ \c -> done c
  mplus l r = CPS $ \c -> choice c l r

-- instance ReaderM CPSComp TReader where
--   ask = CPS $ \c -> yield c (env c)
--   
-- instance RunReaderM CPSComp TReader where
--   local i (CPS m) = CPS $ \c -> m c{ env = i }

instance ExceptionM CPSComp TException where
  raise e = CPS $ \c -> throwC c e

instance RunExceptionM CPSComp TException where
  try (CPS m) = CPS $ \c -> m $
    c{ yield = \a -> yield c (Right a)
     , throwC = \e -> yield c (Left e)
     , choice = \l r -> choice c (try l) (try r)
     }

instance RunMonadPlus CPSComp where
  msplit (CPS m) = CPS $ \c -> 
    let c1 = c{
         yield  = \a -> yield c (Just (a, mzero))
       , done   = yield c Nothing
       , choice = 
         let ch l r = runCPS l $ c{
            yield = \a -> yield c (Just (a, r))
          , done  = runCPS r c1
          , choice = \ll lr -> ch ll (mplus lr r)
          }
         in ch
       }
    in m c1
   
instance Moiell CPS where
  
  urObject = return.O $ Ur

  -- object :: CPS -> CompMap -> CompMap -> CPS -> CPS
  object parComp attrs _ content = do
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

  -- apply :: CPS -> CPS -> CPS
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
  
  throw e = (e >>= raise) >> mzero
  c `catch` h = try c >>= either (apply h . return) return
  fatal = fail

  this = do
    env <- ask
    return.O $ head env
  
  inParent c = do
    env <- ask
    local (tail env) c

  
  -- run :: CPS -> String
  run = showResult . runWithEnv globalObject



evalAttr :: TIdent -> Object -> CPS
evalAttr attrName obj = local (obj : oEnv obj) (lookupAttr attrName obj)

lookupAttr :: TIdent -> Object -> CPS
lookupAttr attrName Ur = fail ("Could not find attribute: " ++ attrName)
lookupAttr attrName obj = Map.findWithDefault (lookupAttr attrName $ parent obj) attrName $ attrs obj

setAttr :: TIdent -> CPS -> Object -> Object
setAttr attrName attrValue obj = obj{ attrs = Map.insert attrName attrValue $ attrs obj }


toDouble :: Value -> Comp Double
toDouble (N n) = return n
toDouble (S s) = maybe mzero (return . fst) (listToMaybe (reads s))
toDouble _     = mzero

toString :: Value -> Comp String
toString (S s) = return s
toString v     = return $ show v

mkFun :: (Value -> Comp a) -> (a -> CPS) -> CPS
mkFun fx f = object urObject Map.empty Map.empty $ this >>= (\(O o) -> evalAttr "_" o) >>= fx >>= f


globalObject :: Object
globalObject = Ur

runWithEnv :: Object -> CPS -> TResult
runWithEnv globalScope = flip runCPS c . runReaderT [globalScope] where
  c = CPSData 
        { yield  = \a -> [Right a]
        , done   = [] 
        , throwC = \a -> [Left a]
        , choice = \l r -> runCPS l c ++ runCPS r c
        }

showResult :: TResult -> String
showResult = unlines . map (either (("Err: " ++) . show) show)

instance Show Value where
  show (N n) = show n
  show (S s) = show s
  show (O o) = show o
  show (A i) = "{Attribute " ++ i ++ "}" 
  
instance Show Object where
  show Ur = "{}"
  show (Object par prps e) = show par ++ "{" ++ show (Map.keys prps) ++ "}"