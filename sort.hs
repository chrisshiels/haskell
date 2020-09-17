-- 'sort.hs'.


import Data.Function
import Data.List
import System.Environment
import System.Exit


sort :: String -> String
sort s = lines s &
         Data.List.sort &
         unlines


app :: [ String ] -> IO ExitCode
app [] = getContents >>= return . Main.sort >>= putStr >>
         return ExitSuccess
app fs = mapM readFile fs >>= return . Main.sort . unlines >>= putStr >>
         return ExitSuccess


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
