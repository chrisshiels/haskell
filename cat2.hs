-- 'cat2.hs'.


import System.Environment
import System.Exit
import System.IO


cat :: Handle -> Handle -> IO Handle
cat hout hin = hGetContents hin >>= hPutStr hout >>
               return hin


catfilename :: Handle -> String -> IO ()
catfilename hout fin = openFile fin ReadMode >>= cat stdout >>= hClose


app :: [ String ] -> IO ExitCode
app [] = cat stdout stdin >>
         return ExitSuccess
app fs = mapM (catfilename stdout) fs >>
         return ExitSuccess


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
