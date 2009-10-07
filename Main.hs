import Moiell

main :: IO ()
main = runFile "test.moi" >>= putStr . showResult

r :: String -> IO ()
r = putStr . showResult . runString

f :: String -> IO ()
f n = runFile (n ++ ".moi") >>= putStr . showResult
