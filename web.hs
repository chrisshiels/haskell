-- 'web.hs'.


{-# LANGUAGE OverloadedStrings #-}


import System.Environment
import System.Exit
import Web.Scotty


web :: Int -> IO ()
web port =
  scotty port $
    get "/:word" $
      do word <- param "word"
         text word


app :: [ String ] -> IO ExitCode
app [] = web 3000 >>
         return ExitSuccess


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
