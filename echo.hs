-- 'echo.hs'.


import System.Environment
import System.Exit


echo :: [ String ] -> IO ExitCode
echo args = (putStrLn . unwords $ args) >>
            return ExitSuccess


app :: [ String ] -> IO ExitCode
app args = echo args


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
