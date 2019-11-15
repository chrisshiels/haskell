-- 'datesvr.hs'.


{-# LANGUAGE OverloadedStrings #-}


import Control.Monad.IO.Class
import Data.Map
import Data.Text
import Data.Time.Calendar
import Data.Time.Clock
import System.Environment
import System.Exit
import Web.Scotty


getUTCTime :: IO UTCTime
getUTCTime = getCurrentTime


getDateTuple :: UTCTime -> (Integer, Int, Int)
getDateTuple = toGregorian . utctDay


getDateMap :: (Integer, Int, Int) -> Map Text Int
getDateMap (y, m, d) = fromList [ ("year"  ::Text, fromInteger y),
                                  ("month" ::Text, m),
                                  ("day"   ::Text, d) ]


home :: ActionM ()
home = liftIO (getUTCTime >>=
               return . getDateTuple >>=
               return . getDateMap) >>=
       Web.Scotty.json


web :: Int -> IO ()
web port = scotty port $
             get "/" home


app :: [ String ] -> IO ExitCode
app [] = web 3000 >>
         return ExitSuccess


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
