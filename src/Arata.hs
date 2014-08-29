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

{-# LANGUAGE LambdaCase #-}

module Arata where

import Data.Time.Clock.POSIX
import Control.Monad.State
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import System.IO.Error
import Network.Connection
import Arata.Types
import Arata.Message
import Arata.Helper
import Arata.Protocol.Charybdis

--sid = "0AR"
host' = "127.0.0.1"
port = 6667
password = "password"
--remotePassword = "password"
vHost' = "services.int"

run :: IO ()
run = forever $ do
    tryIOError $ bracket connect disconnect (evalStateT runLoop . defaultEnv)
    threadDelay 3000000

connect :: IO Connection
connect = do
    putStrLn ("Connecting to `" ++ host' ++ ':' : show port ++ "`")
    ctx <- initConnectionContext
    connectTo ctx (ConnectionParams host' port Nothing Nothing)

disconnect :: Connection -> IO ()
disconnect con = do
    putStrLn "Disconnected"
    connectionClose con

runLoop :: Arata ()
runLoop = protoRegister >> loop

loop :: Arata ()
loop = forever $ recv >>= \case
    Just line -> do
        liftIO $ putStrLn ("-> " ++ line)
        handleMessage (parseMessage line)
    Nothing   -> return ()

handleMessage :: Message -> Arata ()
handleMessage (Message _ _ "PASS" (pass:"TS":"6":_)) = unless (pass == password) $ do
    protoDisconnect "Invalid password"
    fail "Invalid password"
handleMessage (Message _ _ "SERVER" _) = do
    ts <- liftIO (fmap round getPOSIXTime)
    send ("SVINFO 6 6 0 :" ++ show ts)
    burst
handleMessage (Message _ _ "PING" (server1:_)) = send ("PONG :" ++ server1)
handleMessage _ = return ()

burst :: Arata ()
burst = do
    protoIntroduceClient $ Client 1 "ChanServ" 1 "ChanServ" "Channel Services"  "Sio" vHost' "127.0.0.1" "127.0.0.1" Nothing
    protoIntroduceClient $ Client 2 "NickServ" 1 "NickServ" "Nickname Services" "Sio" vHost' "127.0.0.1" "127.0.0.1" Nothing
