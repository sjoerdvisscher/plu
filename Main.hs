import Moiell
import Moiell.Semantics
import Moiell.Serialize

main :: IO ()
main = f "test"

r :: String -> IO ()
r s = putStr $ run (compileString s :: Comp Value)

f :: String -> IO ()
f n = (compileFile (n ++ ".moi") :: IO (Comp Value)) >>= putStr . run

dr :: String -> IO ()
dr s = putStrLn $ run (compileString s :: String)

df :: String -> IO ()
df n = (compileFile (n ++ ".moi") :: IO String) >>= putStrLn . run
