-- 'collatz.hs'.


import Data.List
import System.Environment
import System.Exit
import Text.Read


collatz :: Integral a => a -> a
collatz n | n `mod` 2 == 0 = n `div` 2
collatz n                  = 3 * n + 1


collatzlist :: Integral a => a -> [ a ]
collatzlist n | n == 1    = [ 1 ]
collatzlist n             = n : collatzlist (collatz n)


output :: (Integral a, Show a) => a -> IO ExitCode
output n = (putStrLn . intercalate "\n" . map show . collatzlist $ n) >>
           return ExitSuccess


usage :: ExitCode -> IO ExitCode
usage e = putStrLn "Usage:  collatz n" >>
          return e


app :: [ String ] -> IO ExitCode
app [ "--help" ] = usage ExitSuccess
app []           = usage ExitSuccess
app [ n ]        = case readMaybe n :: Maybe Int of
                   Just n  -> output n
                   Nothing -> usage (ExitFailure 1)
app _            = usage (ExitFailure 1)


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
