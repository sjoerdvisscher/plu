{-# OPTIONS -fglasgow-exts #-}  
{-# LANGUAGE NoMonomorphismRestriction #-}
module Moiell.Semantics where

import Prelude hiding (foldr, mapM, sequence)
import Data.Foldable
import Data.Maybe
import Data.Traversable
import qualified Data.Map as Map
import Control.Monad hiding (msum, mapM, sequence)
import Control.Monad.Error hiding (msum, mapM, sequence, lift)
import Control.Monad.Reader hiding (msum, mapM, sequence, lift)
import Control.Applicative

type TEnv = Map.Map String (Result Value)
type TError = String
type TObject a = (TEnv, Stream a)

data StateRec = StateRec { env :: TEnv }

data Result a = Yield a (Stream a) StateRec | Done StateRec | Error TError StateRec
data Stream a = Str (StateRec -> Result a)

data Value = N Double | S String | C Char | O (TObject Value)

eval s (Str c)   = c s
compAp f (Str c) = Str $ f . c

instance Monad Stream where
  return x  = Str $ Yield x empty
  fail s    = Str $ Error s
  c >>= f   = compAp helper c where
    helper (Yield x sx s) = eval s $ (f x) <|> (sx >>= f)
    helper (Done s)       = Done s
    helper (Error e s)    = Error e s

instance MonadPlus Stream where
  mzero     = Str $ Done
  mplus a b = compAp helper a where
    helper (Yield x sa s) = Yield x (mplus sa b) s
    helper (Done s)       = eval s b 
    helper r              = r
  
instance MonadReader TEnv Stream where
  ask       = Str $ \s -> Yield (env s) empty s
  local f c = Str $ \s -> restoreEnv s $ eval s{ env = f (env s) } c where
    restoreEnv s (Yield x sa s1) = Yield x (compAp (restoreEnv s) sa) s1
    restoreEnv s (Done s1)       = Done s1 { env = env s }
    restoreEnv s (Error e s1)    = Error e s1 { env = env s }

instance MonadError TError Stream where
  throwError errMsg = Str $ Error errMsg
  catchError c hnd  = compAp helper c where
    helper (Error e s) = eval s (hnd e)
    helper r           = r

instance Functor Stream where
  fmap  = liftM

instance Applicative Stream where
  pure  = return
  (<*>) = ap

instance Alternative Stream where
  empty = mzero
  (<|>) = mplus

instance Functor Result where
  fmap f (Yield x c s) = Yield (f x) (fmap f c) s
  fmap f (Done s)      = Done s
  fmap f (Error e s)   = Error e s

instance Show Value where
  show (N n) = show n
  show (S s) = s
  show (C c) = [c]
  show (O o) = "{Object}"

addLocals localEnv = local (localEnv `Map.union`)
addLocal i s = addLocals (Map.singleton i s)

lookupVar :: String -> Stream Value
lookupVar i = do
  env <- ask
  maybe (fail ("Undeclared variable: " ++ i)) (Str . const) $ Map.lookup i env

thunk :: Stream a -> Stream (Result a)
thunk c = Str $ \s -> Yield (eval s c) empty s

apply :: Stream Value -> Stream Value -> Stream Value
apply ops args = do
  val <- ops
  argsThunk <- thunk args
  case val of
    O (props, expr) -> addLocals props $ addLocal "_" argsThunk expr
    x               -> fail ("Cannot apply a non-object: " ++ show x)

object props val = Str $ \s -> Yield (O (fmap (eval s) props,val)) empty s

lift = object Map.empty
unit = lift unit

mkFun fx f = lift $ do
  x <- fx $ lookupVar "_"
  f x

mkFun2 fx fy f = lift $ do
  x <- fx $ lookupVar "_"
  lift $ do
    y <- fy $ lookupVar "_"
    f x y
    
mkBinOp op = mkFun2 toDouble toDouble (\l r -> pure . N $ op l r)

startState = StateRec { env = globalEnv }
globalEnv = fmap (eval startState) $ Map.fromList [
    ("unit", unit),
    ("Each", eachS),
    ("Head", headS),
    ("Tail", tailS),
    ("And",  andS),
    ("Or",   orS),
    ("Not",  notS),
    ("+", mkBinOp (+)),
    ("-", mkBinOp (-)),
    ("*", mkBinOp (*)),
    ("/", mkBinOp (/)),
    ("mod", mkBinOp (\l r -> fromIntegral (floor l `mod` floor r)))
  ]

eachS = mkFun2 id id (\body arg -> apply (pure body) (pure arg))

headS = mkFun splitS fst
tailS = mkFun splitS snd

splitS :: Stream a -> Stream (Stream a, Stream a)
splitS comp = compAp helper comp where
  helper (Yield x sa s) = Yield (pure x, sa) empty s
  helper (Done s)       = Yield (empty, empty) empty s
  helper (Error e s)    = Error e s

ifS testComp trueComp falseComp = compAp helper testComp where
  helper (Yield _ _ s) = eval s trueComp
  helper (Done s)      = eval s falseComp
  helper (Error e s)   = Error e s

notS = lift $ ifS (lookupVar "_") empty unit
andS = lift $ let aStr = lookupVar "_" in lift $ let bStr = lookupVar "_" in ifS aStr bStr empty
orS  = lift $ let aStr = lookupVar "_" in lift $ let bStr = lookupVar "_" in ifS aStr aStr bStr

toDouble vStr = do
  v <- vStr
  case v of
    N n -> pure n
    S s -> maybe empty (pure . fst) (listToMaybe (reads s))
    C c -> maybe empty (pure . fst) (listToMaybe (reads [c]))
    O o -> empty

{-
ifS testComp trueComp falseComp = compAp helper testComp where
  helper (Yield _ _ s) = eval s trueComp
  helper (Done s)      = eval s falseComp
  helper (Error e s)   = Error e s

oneS str = do (x, xs) <- splitS str; x
notS str = ifS str empty (return ())
andS aStr bStr = ifS aStr bStr empty
orS aStr bStr = ifS aStr aStr bStr

toDouble vStr = do
  v <- vStr
  case v of
    N n -> pure n
    S s -> maybe empty (pure . fst) (listToMaybe (reads s))

toString vStr = do
  v <- vStr
  case v of
    N n -> pure (show n)
    S s -> pure s

filterN2 :: (a -> b -> Bool) -> Stream a -> Stream b -> Stream a
filterN2 f aStr bStr = do
  a <- aStr
  b <- bStr
  if (f a b) then (pure a) else empty

(*<=*) = filterN2 (<=)
(*>=*) = filterN2 (>=)
(*<*)  = filterN2 (<)
(*>*)  = filterN2 (>)
(*==*) = filterN2 (==)
(*/=*) = filterN2 (/=)

liftN2 f aStr bStr = do
  a <- toDouble aStr
  b <- toDouble bStr
  pure . N $ f a b

liftS2 f aStr bStr = do
  a <- toString aStr
  b <- toString bStr
  pure . S $ f a b
  
(*+*)  = liftN2 (+)
(*-*)  = liftN2 (-)
(***)  = liftN2 (*)
(*++*)  = liftS2 (++)
-}