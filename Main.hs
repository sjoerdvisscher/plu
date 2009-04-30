import Moiell

main :: IO ()
main = runFile "test.moi" >>= putStr . showResult

r :: String -> IO ()
r = putStr . showResult . runString