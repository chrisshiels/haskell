-- 'expr.hs'.


import System.Environment
import System.Exit
import System.IO
import Text.Parsec


parser :: Parsec String () Integer
parser =
    do { x <- expression; eof; return x }


expression :: Parsec String () Integer
expression =
    try (do { x <- term; _token '+'; y <- expression; return (x + y) }) <|>
    try (do { x <- term; _token '-'; y <- expression; return (x - y) }) <|>
    term


term :: Parsec String () Integer
term =
    try (do { x <- primary; _token '*'; y <- term; return (x * y) }) <|>
    try (do { x <- primary; _token '/'; y <- term; return (x `div` y) }) <|>
    primary


primary :: Parsec String () Integer
primary =
    try (do { _token '('; x <- expression; _token ')'; return x }) <|>
    integer


integer :: Parsec String () Integer
integer =
    do { spaces; fmap read (many1 digit) }


_token :: Char -> Parsec String () Char
_token ch = do { spaces; char ch }


_parse :: String -> IO ExitCode
_parse s = case parse parser "expr" s of
           Left message -> print message >>
                           return (ExitFailure 1)
           Right value  -> print value >>
                           return ExitSuccess


loop :: IO ExitCode
loop = do eof <- isEOF
          if eof
          then return ExitSuccess
          else do getLine >>= _parse
                  loop


app :: [ String ] -> IO ExitCode
app []   = loop
app args = _parse (unwords args)


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
