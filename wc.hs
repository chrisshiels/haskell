-- 'wc.hs'.


import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.Printf


data Flag = LinesFlag | WordsFlag | CharsFlag | HelpFlag
            deriving (Eq, Show)


type Filename = String
type Content = String
type Lines = Int
type Words = Int
type Chars = Int


content :: Filename -> IO (Filename, Content)
content f = (if f == "-" then getContents else readFile f) >>= (return . (,) f)


contents :: [ Filename ] -> IO [ (Filename, Content) ]
contents fs = mapM content fs


count :: (Filename, Content) -> (Filename, Lines, Words, Chars)
count (f, s) = (f,
                length . lines $ s,
                length . words $ s,
                length s)


counts :: [ (Filename, Content) ] -> [ (Filename, Lines, Words, Chars) ]
counts ts = map count ts


totals :: [ (Filename, Lines, Words, Chars) ] ->
          [ (Filename, Lines, Words, Chars) ]
totals [ c ] = [ c ]
totals cs    = cs ++ [ foldl sum ("total", 0, 0, 0) cs ]
               where sum (af, al, aw, ac) (_, el, ew, ec) =
                         (af, al + el, aw + ew, ac + ec)


result :: [ Flag ] ->
          (Filename, Lines, Words, Chars) ->
          IO (Filename, Lines, Words, Chars)
result flags (f, l, w, c) =
    (if LinesFlag `elem` flags then printf "%5d"   l else return ()) >>
    (if WordsFlag `elem` flags then printf "  %5d" w else return ()) >>
    (if CharsFlag `elem` flags then printf "  %5d" c else return ()) >>
    (if f /= "-"               then printf "  %s"  f else return ()) >>
    printf "\n" >>
    return (f, l, w, c)


results :: [ Flag ] -> [ (Filename, Lines, Words, Chars) ] -> IO ExitCode
results flags rs = mapM (result flags) rs >>
                   return ExitSuccess


wordcounts :: [ Flag ] -> [ String ] -> IO ExitCode
wordcounts flags filenames = contents filenames >>=
                             return . counts >>=
                             return . totals >>=
                             results flags


app :: [ Flag ] -> [ String ] -> IO ExitCode
app flags []        = wordcounts flags [ "-" ]
app flags filenames = wordcounts flags filenames


optdescr :: [ OptDescr Flag ]
optdescr = [
               Option [ 'l' ]
                      [ "lines" ]
                      (NoArg LinesFlag)
                      "Line count",
               Option [ 'w' ]
                      [ "words" ]
                      (NoArg WordsFlag)
                      "Word count",
               Option [ 'c' ]
                      [ "chars" ]
                      (NoArg CharsFlag)
                      "Char count",
               Option [ 'h' ]
                      [ "help" ]
                      (NoArg HelpFlag)
                      "Help"
           ]


usage :: [ OptDescr Flag ] -> ExitCode -> IO ExitCode
usage optdescr e =
    putStr (usageInfo "Usage:  wc [ options ] [ file ... ]" optdescr) >>
    return e


parse :: [ OptDescr Flag ] ->
         ([ OptDescr Flag ] -> ExitCode -> IO ExitCode) ->
         ([ Flag ] -> [ String ] -> IO ExitCode) ->
         [ String ] ->
         IO ExitCode
parse optdescr usage app args =
    case getOpt RequireOrder optdescr args of
    (flags, args, []) | HelpFlag `elem` flags -> usage optdescr ExitSuccess
    ([], args, [])                            -> app [ LinesFlag,
                                                       WordsFlag,
                                                       CharsFlag ] args
    (flags, args, [])                         -> app flags args
    (_, _, errs)                              -> putStrLn "Errors:" >>
                                                 putStrLn (concat errs) >>
                                                 usage optdescr (ExitFailure 1)


main :: IO ExitCode
main = getArgs >>= parse optdescr usage app >>= exitWith
