import Moiell
import Moiell.Semantics

main :: IO ()
main = f "test"

r :: String -> IO ()
r s = putStr $ run (compileString s :: Comp Value)

f :: String -> IO ()
f n = (compileFile (n ++ ".moi") :: IO (Comp Value)) >>= putStr . run
