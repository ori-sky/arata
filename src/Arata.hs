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
    catchIOError (bracket (connect cp) disconnect (f cp)) print
    threadDelay 3000000
  where f cp con = evalStateT (setEnvConfigParser cp >> runLoop) (defaultEnv con burst')

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
handleMessage (Message _ _ "PING" (server1:_)) = send ("PONG :" ++ server1)
handleMessage m = protoHandleMessage m

burst' :: Arata ()
burst' = do
    csNick <- getConfig "chanserv" "nick"
    csUser <- getConfig "chanserv" "user"
    csHost <- getConfig "chanserv" "host"
    csName <- getConfig "chanserv" "name"
    nsNick <- getConfig "nickserv" "nick"
    nsUser <- getConfig "nickserv" "user"
    nsHost <- getConfig "nickserv" "host"
    nsName <- getConfig "nickserv" "name"
    protoIntroduceClient 1 csNick csUser csName csHost Nothing (Just csHandler)
    protoIntroduceClient 2 nsNick nsUser nsName nsHost Nothing Nothing
    return ()

csHandler :: PrivmsgH
csHandler src dst ("HELP":_) = protoNotice dst src "Not implemented yet"
csHandler src dst _ = protoNotice dst src "Invalid command. Use \x02/msg ChanServ HELP\x02 for a list of valid commands."
