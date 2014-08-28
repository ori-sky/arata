{- Copyright 2014 David Farrell <shokku.ra@gmail.com>

 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at

 - http://www.apache.org/licenses/LICENSE-2.0

 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX
import Control.Monad.State
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import System.IO
import System.IO.Error
import Network
import Arata.Types
import Arata.Message

sid = "0AR"
host = "127.0.0.1"
port = 6667
password = "password"
remotePassword = "password"
vHost = "services.shockk.co.uk"

data Env = Env { handle :: Handle }
type Arata = StateT Env IO
defaultEnv h = Env { handle = h }

main :: IO ()
main = forever $ do
    tryIOError $ bracket connect disconnect (evalStateT run . defaultEnv)
    threadDelay 3000000

connect :: IO Handle
connect = do
    putStrLn ("Connecting to `" ++ host ++ ':' : show port ++ "`")
    h <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hSetNewlineMode h (NewlineMode CRLF CRLF)
    return h

disconnect :: Handle -> IO ()
disconnect handle = do
    putStrLn "Disconnected"
    hClose handle

run :: Arata ()
run = do
    send ("PASS " ++ remotePassword ++ " TS 6 :" ++ sid)
    send "CAPAB :ENCAP QS EX IE EUID"
    send "SERVER services.int 1 :Arata IRC Services"
    loop

loop :: Arata ()
loop = forever $ do
    line <- gets handle >>= liftIO . hGetLine
    liftIO $ putStrLn (host ++ " -> " ++ line)
    handleMessage (parseMessage line)

send :: String -> Arata ()
send line = do
    liftIO $ putStrLn (host ++ " <- " ++ line)
    h <- gets handle
    liftIO $ hPutStrLn h line

handleMessage :: Message -> Arata ()
handleMessage (Message _ _ "PASS" (pass:"TS":"6":_)) = unless (pass == password) $ do
    send ("SQUIT " ++ sid ++ " :Invalid password")
    fail "Invalid password"
handleMessage (Message _ _ "SERVER" _) = do
    ts <- liftIO (fmap round getPOSIXTime)
    send ("SVINFO 6 6 0 :" ++ show ts)
    burst
handleMessage (Message _ _ "PING" (server1:_)) = send ("PONG :" ++ server1)
handleMessage _ = return ()

burst :: Arata ()
burst = do
    introduceClient "ChanServ" 1 "Sio" "ChanServ" vHost "127.0.0.1" (sid ++ "000001") "127.0.0.1" Nothing "Channel Services"
    introduceClient "NickServ" 1 "Sio" "NickServ" vHost "127.0.0.1" (sid ++ "000001") "127.0.0.1" Nothing "Nickname Services"

-- introduceClient nick ts umodes vHost host uid ip account realName
introduceClient :: Show a => String -> a -> String -> String -> String -> String -> String -> String -> Maybe String -> String -> Arata ()
introduceClient nick ts umodes user vHost host uid ip account realName = send line
  where line = ':' : sid ++ " EUID " ++ nick ++ " 1 " ++ show ts ++ " +" ++ umodes ++ ' ' : user ++ ' ' : vHost
            ++ ' ' : host ++ ' ' : uid ++ ' ' : ip ++ ' ' : fromMaybe "*" account ++ " :" ++ realName
