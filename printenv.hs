-- 'printenv.hs'.


import Data.List
import Data.Maybe
import System.Environment
import System.Exit


getenvironmentall :: IO [ (String, String) ]
getenvironmentall = getEnvironment


sorttuples :: [ (String, String) ] -> [ (String, String) ]
sorttuples ts = sortOn fst ts


jointuples :: [ (String, String) ] -> [ String ]
jointuples ts = map jointuple ts
                where jointuple (a, b) = a ++ "=" ++ b


getenvironmentvalues :: [ String ] -> IO [ Maybe String ]
getenvironmentvalues ns = mapM lookupEnv $ ns


filtermaybes :: [ Maybe a ] -> [ Maybe a ]
filtermaybes ms = filter isJust ms


mapmaybes :: [ Maybe a ] -> [ a ]
mapmaybes ms = map fromJust ms


printenv :: [ String ] -> IO ExitCode
printenv [] = getenvironmentall >>=
              mapM putStrLn . jointuples . sorttuples >>
              return ExitSuccess
printenv ns = getenvironmentvalues ns >>=
              mapM putStrLn . mapmaybes . filtermaybes >>
              return ExitSuccess


usage :: ExitCode -> IO ExitCode
usage e = putStrLn "Usage:  printenv [ variable ... ]" >>
          return e


app :: [ String ] -> IO ExitCode
app [ "--help" ] = usage ExitSuccess
app as           = printenv as


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
