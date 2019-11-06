-- 'speller.hs'.


import System.Environment
import System.Exit


spell :: String -> String
spell     [] = ""
spell (c:cs) = c : " is for " ++ (c:cs)


speller :: [ String ] -> String
speller []         = ""
speller [ s ]      = spell s
speller [ s1, s2 ] = spell s1 ++ ", and " ++ spell s2
speller (s:ss)     = spell s ++ ", " ++ speller ss


app :: [ String ] -> IO ExitCode
app args = putStrLn (speller args) >>
           return ExitSuccess


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
