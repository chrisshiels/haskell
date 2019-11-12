-- 'wordfrequencies1.hs'.


import Data.Function
import Data.List
import System.Exit
import Text.Printf


frequencies :: String -> [(Int, String)]
frequencies s = words s &
                sort &
                group &
                map totuple &
                sortBy comparetuples
                where totuple e = (length e, head e)
                      comparetuples x y = compare (fst y) (fst x)


output :: [(Int, String)] -> IO ExitCode
output ts = mapM (\e -> printf "%-10d %s\n" (fst e) (snd e)) ts >>
            return ExitSuccess


main = getContents >>=
       return . frequencies >>=
       output
