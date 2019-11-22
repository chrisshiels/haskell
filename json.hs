-- 'json.hs'.


{-# LANGUAGE OverloadedStrings #-}


import Control.Applicative
import Data.Aeson


data Date = Date {
              day   :: Int,
              month :: Int,
              year  :: Int
            } deriving Show


instance FromJSON Date where
  parseJSON (Object v) =
    Date <$> v .: "day" <*> v .: "month" <*> v .: "year"
  parseJSON _ = empty


instance ToJSON Date where
  toJSON (Date day month year) =
    object [ "day" .= day, "month" .= month, "year" .= year ]


main :: IO ()
main =
  do putStrLn $ "Encode: " ++
              (show (encode (Date { day = 1, month = 1, year = 1970 })))
     putStrLn $ "Decode: " ++
              (show (decode "{ \"day\": 1, \"month\": 1, \"year\": 1970 }" :: Maybe Date))
     putStrLn $ "Decode: " ++
              (show (decode "{ \"zday\": 1, \"zmonth\": 1, \"zyear\": 1970 }" :: Maybe Date))
