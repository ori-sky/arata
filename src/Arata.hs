{- Copyright 2014-2015 David Farrell <shokku.ra@gmail.com>

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
import qualified Arata.NickServ.Add as NSAdd
import qualified Arata.NickServ.Drop as NSDrop
import qualified Arata.NickServ.Help as NSHelp
import qualified Arata.NickServ.Login as NSLogin
import qualified Arata.NickServ.Logout as NSLogout
import qualified Arata.NickServ.Register as NSRegister

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
burst' = mapM_ handleExport (concat [NS.exports, CS.exports, NSHelp.exports, NSAdd.exports, NSDrop.exports, NSLogin.exports, NSLogout.exports, NSRegister.exports])

handleExport :: PluginExport -> Arata ()
handleExport (ServExport s) = do
    name <- getConfig "info" "name"
    sect <- getSection s
    uid <- gets nextUid
    let nick = sect "nick"
    protoIntroduceClient uid nick (sect "user") (sect "name") name Nothing (Just (handler s nick))
    modify (\env -> env { nextUid = uid + 1 })
handleExport (CommandExport s c) = addCommand s c

handler :: String -> String -> PrivmsgH
handler s nick src dst (x:xs) = do
    getCommand s x >>= \case
        Nothing  -> protoNotice dst src ("Invalid command. Use \2/msg " ++ nick ++ " HELP\2 for a list of valid commands.")
        Just cmd -> commandH cmd src dst xs
handler _ _ _ _ [] = return ()
