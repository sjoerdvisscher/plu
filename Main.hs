import Moiell
import Moiell.MonadInstance
import Moiell.Semantics
import Moiell.CPS
import Moiell.JS
import Moiell.Serialize

main :: IO ()
main = f "test"

r :: String -> IO ()
r s = putStr $ run (compileString s :: M Comp)

f :: String -> IO ()
f n = (compileFile ("examples/" ++ n ++ ".moi") :: IO (M Comp)) >>= putStr . run

cr :: String -> IO ()
cr s = putStr $ run (compileString s :: M CPS)

cf :: String -> IO ()
cf n = (compileFile ("examples/" ++ n ++ ".moi") :: IO (M CPS)) >>= putStr . run

jr :: String -> IO ()
jr s = putStrLn $ run (compileString s :: JS)

jf :: String -> IO ()
jf n = (compileFile ("examples/" ++ n ++ ".moi") :: IO JS) >>= putStrLn . run

dr :: String -> IO ()
dr s = putStrLn $ run (compileString s :: Src)

df :: String -> IO ()
df n = (compileFile ("examples/" ++ n ++ ".moi") :: IO Src) >>= putStrLn . run
