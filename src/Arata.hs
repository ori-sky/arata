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

import Data.ConfigFile.Monadic
import Data.Time.Clock.POSIX
import Control.Monad.State
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import System.IO.Error
import Network.Connection
import Arata.Types
import Arata.Config
import Arata.Message
import Arata.Helper
import Arata.Protocol.Charybdis

vHost' = "services.int"

run :: IO ()
run = forever $ do
    cp <- loadConfig' "arata.conf"
    tryIOError $ bracket (connect cp) disconnect (f cp)
    threadDelay 3000000
  where f cp = evalStateT (setEnvConfigParser cp >> runLoop) . defaultEnv

connect :: ConfigParser -> IO Connection
connect cp = do
    putStrLn ("Connecting to `" ++ host' ++ ':' : show port ++ "`")
    ctx <- initConnectionContext
    connectTo ctx (ConnectionParams host' (fromIntegral port) tlsSettings Nothing)
  where host' = getConfig' cp "remote" "host"
        port  = getConfig' cp "remote" "port" :: Int
        tls   = getConfig' cp "remote" "tls"  :: Bool
        tlsSettings = if tls then Just (TLSSettingsSimple True False False) else Nothing

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
handleMessage (Message _ _ "PASS" (pass:"TS":"6":_)) = do
    password <- getConfig "local" "password"
    unless (pass == password) $ do
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
    csNick <- getConfig "chanserv" "nick"
    csUser <- getConfig "chanserv" "user"
    csHost <- getConfig "chanserv" "host"
    csName <- getConfig "chanserv" "name"
    nsNick <- getConfig "nickserv" "nick"
    nsUser <- getConfig "nickserv" "user"
    nsHost <- getConfig "nickserv" "host"
    nsName <- getConfig "nickserv" "name"
    protoIntroduceClient $ Client 1 csNick 1 csUser csName "Sio" csHost "127.0.0.1" "127.0.0.1" Nothing
    protoIntroduceClient $ Client 2 nsNick 1 nsUser nsName "Sio" nsHost "127.0.0.1" "127.0.0.1" Nothing
