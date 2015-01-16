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

import Data.Char (toUpper)
import Data.ConfigFile.Monadic
import Data.Acid
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
import qualified Arata.NickServ as NS
import qualified Arata.ChanServ as CS

run :: IO ()
run = do
    cp <- loadConfig' "arata.conf"
    as <- openLocalStateFrom "db" defaultDBState
    let f cp con = evalStateT (setEnvConfigParser cp >> runLoop) (defaultEnv con burst' as)
    forever $ do
        catchIOError (bracket (connect cp) disconnect (f cp)) print
        threadDelay 3000000

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
handleMessage = protoHandleMessage

burst' :: Arata ()
burst' = do
    name <- getConfig "info" "name"
    ns <- NS.serv
    cs <- CS.serv
    protoIntroduceClient 1 (servNick ns) (servUser ns) (servRealName ns) name Nothing (Just (handler (servHandler ns)))
    protoIntroduceClient 2 (servNick cs) (servUser cs) (servRealName cs) name Nothing (Just (handler (servHandler cs)))
    --protoIntroduceClient 2 (ns "nick") (ns "user") (ns "name") (ns "host") Nothing (Just nsHandler)
    --protoIntroduceClient 2 (cs "nick") (cs "user") (cs "name") name Nothing (Just csHandler)
    return ()

-- stage 1 handlers

handler :: Maybe PrivmsgH -> PrivmsgH
handler (Just h) src dst (x:xs) = h src dst (map toUpper x : xs)
handler (Just h) src dst [] = h src dst []
handler Nothing _ _ _ = return ()

-- stage 2 handlers

csHandler' :: PrivmsgH
csHandler' src dst ("HELP":_) = protoNotice dst src "Not implemented"
csHandler' src dst _ = do
    nick' <- getConfig "chanserv" "nick"
    protoNotice dst src ("Invalid command. Use \2/msg " ++ nick' ++ " HELP\2 for a list of valid commands.")
