-- 'cat.hs'.


import System.Environment
import System.Exit


cat :: String -> IO ()
cat "-" = getContents >>= putStr
cat f   = readFile f >>= putStr


app :: [ String ] -> IO ExitCode
app [] = cat "-" >>
         return ExitSuccess
app fs = mapM cat fs >>
         return ExitSuccess


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
