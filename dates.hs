-- 'dates.hs'.


import Data.Time
import System.Exit


date1 :: IO String
date1 = do t <- getZonedTime
           let s = formatTime defaultTimeLocale "%c" t
           return s


date2 :: IO String
date2 = fmap (formatTime defaultTimeLocale "%c") getZonedTime


date3 :: IO String
date3 = formatTime defaultTimeLocale "%c" <$> getZonedTime


main :: IO ExitCode
main = date1 >>= putStrLn >>
       date2 >>= putStrLn >>
       date3 >>= putStrLn >>
       exitSuccess
