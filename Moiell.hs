module Moiell where

import Moiell.Parser
import Moiell.Expr
import Moiell.Globals

import Control.Arrow ((***))

runComp :: Comp Value -> IO ()
runComp comp = putStr $ unlines $ concat $ map showResult $ run globalObject comp

showResult :: (Either TException Value, TWriter) -> [String]
showResult = uncurry (:) . (either ("Err: " ++) show *** map ("Log: " ++))
  
runParseResult r = case r of
  (Right ast) -> ast2Comp globalScope $ ast
  (Left e) -> error (show e)
  
p :: String -> Comp Value
p = runParseResult . parseString
t :: String -> IO ()
t = runComp . p

main :: IO ()
main = parseFile "test.moi" >>= runComp . runParseResult
