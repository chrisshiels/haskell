-- 'date.hs'.


import Data.Time
import System.Environment
import System.Exit


date :: IO ExitCode
date = getZonedTime >>=
       return . formatTime defaultTimeLocale "%c" >>=
       putStrLn >>
       return ExitSuccess


usage :: ExitCode -> IO ExitCode
usage e = putStrLn "Usage:  date" >>
          return e


app :: [ String ] -> IO ExitCode
app [] = date
app _  = usage (ExitFailure 1)


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
