-- 'curl.hs'.


import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazy
import Network.Wreq
import System.Environment
import System.Exit


curl :: String -> IO ExitCode
curl url = get url >>=
           ByteStringLazy.putStr . (^. responseBody) >>
           return ExitSuccess


usage :: ExitCode -> IO ExitCode
usage e = putStrLn "Usage:  curl url" >>
          return e


app :: [ String ] -> IO ExitCode
app [ "--help" ] = usage ExitSuccess
app []           = usage ExitSuccess
app [ url ]      = curl url
app _            = usage (ExitFailure 1)


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
