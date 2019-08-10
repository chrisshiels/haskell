-- 'fizzbuzz.hs'.


import System.Environment
import System.Exit
import Text.Printf
import Text.Read


fizzbuzz :: Int -> String
fizzbuzz n | n `mod` 3 == 0 && n `mod` 5 == 0 = "fizzbuzz"
           | n `mod` 3 == 0                   = "fizz"
           | n `mod` 5 == 0                   = "buzz"
           | otherwise                        = show n


fizzbuzzer :: Int -> IO ExitCode
fizzbuzzer 0 = return ExitSuccess
fizzbuzzer n = fizzbuzzer (n - 1) >>
               printf "%-5d %s\n" n (fizzbuzz n) >>
               return ExitSuccess


usage :: ExitCode -> IO ExitCode
usage e = putStrLn "Usage:  fizzbuzz n" >>
          return e


app :: [ String ] -> IO ExitCode
app [ "--help" ] = usage ExitSuccess
app []           = usage ExitSuccess
app [ n ]        = case readMaybe n :: Maybe Int of
                       Just n  -> fizzbuzzer n
                       Nothing -> usage (ExitFailure 1)
app _            = usage (ExitFailure 1)


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
