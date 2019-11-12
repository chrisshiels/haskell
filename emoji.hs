-- 'emoji.hs'.


import Data.Char
import System.Exit


main :: IO ExitCode
main = (putStrLn $ [ chr(0x1F4a9) ]) >> return ExitSuccess
