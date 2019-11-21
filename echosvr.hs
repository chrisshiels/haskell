-- 'echosvr.hs'.


import Control.Exception
import Data.ByteString
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.Exit
import Text.Read


getsocket :: PortNumber -> IO Socket
getsocket port =
  do s <- socket AF_INET Stream defaultProtocol
     setSocketOption s ReuseAddr 1
     bind s (SockAddrInet port 0)
     listen s 1024
     return s


loop :: (Socket -> IO ()) -> Socket -> IO ()
loop action socket =
  do (conn, _) <- accept socket
     action conn
     loop action socket


action :: Socket -> IO ()
action socket =
  do msg <- Network.Socket.ByteString.recv socket 1024
     if Data.ByteString.null msg
     then return ()
     else do Network.Socket.ByteString.sendAll socket msg
             action socket


echosvr :: PortNumber -> IO ExitCode
echosvr port =
  bracket (getsocket port) close (loop action) >>
  return ExitSuccess


usage :: ExitCode -> IO ExitCode
usage e = Prelude.putStrLn "Usage:  echosvr [ port ]" >>
          return e


app :: [ String ] -> IO ExitCode
app [ "--help" ] = usage ExitSuccess
app []           = echosvr 7777
app [ n ]        = case readMaybe n :: Maybe PortNumber of
                   Just n  -> echosvr n
                   Nothing -> usage (ExitFailure 1)
app _            = usage (ExitFailure 1)


main :: IO ExitCode
main = getArgs >>= app >>= exitWith
