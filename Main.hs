import Moiell
import Moiell.Semantics
import Moiell.CPS
import Moiell.Serialize

main :: IO ()
main = f "test"

r :: String -> IO ()
r s = putStr $ run (compileString s :: CPS)

f :: String -> IO ()
f n = (compileFile (n ++ ".moi") :: IO CPS) >>= putStr . run

dr :: String -> IO ()
dr s = putStrLn $ run (compileString s :: Src)

df :: String -> IO ()
df n = (compileFile (n ++ ".moi") :: IO Src) >>= putStrLn . run
