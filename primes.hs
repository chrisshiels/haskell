-- 'primes.hs'.


import Data.List
import System.Environment
import System.Exit
import Text.Read


notfactorof :: Integral a => a -> a -> Bool
notfactorof x y = y `mod` x /= 0


sieve :: Integral a => [ a ] -> [ a ]
sieve [] = []
sieve (x:xs) = [ x ] ++ (sieve . filter (notfactorof x) $ xs)


primes :: Int -> IO ExitCode
primes n = (putStrLn . intercalate "\n" . map show . sieve $ [2..n]) >>
           return ExitSuccess


usage :: ExitCode -> IO ExitCode
usage e = putStrLn "Usage:  primes n" >>
          return e


app :: [ String ] -> IO ExitCode
app [ "--help" ] = usage ExitSuccess
app []           = usage ExitSuccess
app [ n ]        = case readMaybe n :: Maybe Int of
                       Just n  -> primes n
                       Nothing -> usage (ExitFailure 1)
app _            = usage (ExitFailure 1)


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
