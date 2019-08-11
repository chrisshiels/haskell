-- 'ipcalc.hs'.


import Data.Bits
import System.Environment
import System.Exit
import Text.Regex.Posix


type Word = Int
type Bits = Int
data Address = Address Main.Word Main.Bits deriving (Show)


stringtoaddressregexp :: String -> Either String [ String ]
stringtoaddressregexp s =
    case (s =~ "^([0-9]+)\\.([0-9]+)\\.([0-9]+)\\.([0-9]+)/([0-9]+)$") of
        []      -> Left "Bad address"
        [ xs ]  -> Right xs


stringtoaddressread :: [ String ] -> Either String (Int, Int, Int, Int, Int)
stringtoaddressread []                   = Left "Bad address"
stringtoaddressread [ _, a, b, c, d, n ] = Right ((read a :: Int),
                                                  (read b :: Int),
                                                  (read c :: Int),
                                                  (read d :: Int),
                                                  (read n :: Int))


stringtoaddressverify ::
    (Int, Int, Int, Int, Int) -> Either String (Int, Int, Int, Int, Int)
stringtoaddressverify (a, b, c, d, n) =
    if a > 255 || b > 255 || c > 255 || d > 255 || n > 32 then
        Left "Bad address"
    else
        Right (a, b, c, d, n)


stringtoaddressconvert :: (Int, Int, Int, Int, Int) -> Either String Address
stringtoaddressconvert (a, b, c, d, n) = Right (Address (shift a 24 +
                                                         shift b 16 +
                                                         shift c 8 +
                                                         d)
                                                n)


stringtoaddress :: String -> Either String Address
stringtoaddress s = stringtoaddressregexp s >>=
                    stringtoaddressread >>=
                    stringtoaddressverify >>=
                    stringtoaddressconvert


addresstostring :: Address -> String
addresstostring (Address w b) = show (shift w (-24)) ++ "." ++
                                show (shift w (-16) .&. 255) ++ "." ++
                                show (shift w (-8) .&. 255) ++ "." ++
                                show (w .&. 255) ++ "/" ++
                                show b


networkaddress :: Address -> Address
networkaddress (Address w b) = Address (w .&. (shift (2 ^ b - 1) (32 - b))) b


broadcastaddress :: Address -> Address
broadcastaddress a = Address (nw .|. (2 ^ (32 - nb) - 1)) nb
    where (Address nw nb) = networkaddress a


firsthostaddress :: Address -> Address
firsthostaddress a = Address (nw + 1) nb
    where (Address nw nb) = networkaddress a


lasthostaddress :: Address -> Address
lasthostaddress a = Address (bw - 1) bb
    where (Address bw bb) = broadcastaddress a


addressdetails :: Address -> IO ExitCode
addressdetails a =
    putStrLn ("Network:    " ++ addresstostring (networkaddress a) ++ "\n" ++
              "Broadcast:  " ++ addresstostring (broadcastaddress a) ++ "\n" ++
              "First:      " ++ addresstostring (firsthostaddress a) ++ "\n" ++
              "Last:       " ++ addresstostring (lasthostaddress a)) >>
    return ExitSuccess


usage :: ExitCode -> IO ExitCode
usage e = putStrLn "Usage:  ipcalc a.b.c.d/n [ m ]" >>
          return e


app :: [ String ] -> IO ExitCode
app [ "--help" ] = usage ExitSuccess
app []           = usage ExitSuccess
app [ s ]        = case (stringtoaddress s) of
                       (Left message)  -> putStrLn ("Error:  " ++ message) >>
                                          return (ExitFailure 1)
                       (Right address) -> addressdetails address
app _            = usage (ExitFailure 1)


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
