-- 'wordfrequencies2.hs'.


import Data.List
import System.Exit
import Text.Printf


frequencies :: String -> [(Int, String)]
frequencies s = reverse .
                sortOn fst .
                map (\e -> (length e, head e)) .
                group .
                sort .
                words $ s


output :: [(Int, String)] -> IO ExitCode
output ts = mapM (\e -> printf "%-10d %s\n" (fst e) (snd e)) ts >>
            return ExitSuccess


main = getContents >>=
       return . frequencies >>=
       output
