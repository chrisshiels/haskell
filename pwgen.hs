-- 'pwgen.hs'.


import System.Environment
import System.Exit
import System.Random


chars :: String
chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


randomchars :: Int -> String -> StdGen -> String
randomchars n s g = take n .
                    map (s !!) .
                    randomRs (0, (length s - 1)) $
                    g


pwgen :: [ String ] -> IO ExitCode
pwgen args = newStdGen >>=
             return . randomchars 32 chars >>=
             putStrLn >>
             return ExitSuccess


app :: [ String ] -> IO ExitCode
app args = pwgen args


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
