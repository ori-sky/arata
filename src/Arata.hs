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

import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX
import Data.ConfigFile.Monadic
import Data.Acid
import Control.Monad.State
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import System.IO (IOMode(ReadWriteMode))
import System.IO.Error
import qualified Network.Socket as S
import qualified Network.Connection as C
import Arata.Types
import Arata.Config
import Arata.Message
import Arata.Helper
import Arata.Plugin.Load

run :: IO ()
run = do
    cfg <- loadConfig' "arata.conf"
    as <- openLocalStateFrom "db" defaultDBState
    exports <- liftIO (mapM load' (words (getConfig' cfg "info" "plugins"))) >>= return . concat
    let f con = evalStateT runLoop (defaultEnv con as) { configParser = cfg, pluginExports = exports }
    forever $ do
        catchIOError (bracket (connect cfg) disconnect f) print
        threadDelay 3000000
  where load' n = putStrLn ("loading plugin `" ++ n ++ "`") >> load n

connect :: ConfigParser -> IO C.Connection
connect config = do
    putStrLn ("connecting to `" ++ host' ++ ':' : (if tls then "+" else "") ++ show port ++ "`")
    S.getAddrInfo preferred (Just host') (Just (show port)) >>= \case
        []       -> fail "failed to resolve remote address"
        (addr:_) -> do
            sock <- S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)
            S.getAddrInfo preferred (Just $ if localHost == "*" then "0.0.0.0" else localHost)
                                           (if localPort ==  0  then Nothing else Just (show localPort)) >>= \case
                []           -> fail "failed to resolve bind address"
                (bindAddr:_) -> S.bind sock (S.addrAddress bindAddr)
            S.connect sock (S.addrAddress addr)
            hdl <- S.socketToHandle sock ReadWriteMode
            ctx <- C.initConnectionContext
            C.connectFromHandle ctx hdl (C.ConnectionParams host' (fromIntegral port) tlsSettings Nothing)
  where host' = getConfig' config "remote" "host"
        port  = getConfig' config "remote" "port" :: Int
        tls   = getConfig' config "remote" "tls"  :: Bool
        localHost = getConfig' config "local" "host"
        localPort = getConfig' config "local" "port" :: Int
        preferred = Just S.defaultHints { S.addrFlags = [S.AI_ALL] }
        tlsSettings = if tls then Just (C.TLSSettingsSimple True False False) else Nothing

disconnect :: C.Connection -> IO ()
disconnect con = do
    putStrLn "disconnected"
    C.connectionClose con

runLoop :: Arata ()
runLoop = do
    exports <- gets pluginExports
    case [export | export@(ProtocolExport {}) <- exports] of
        []        -> liftIO (putStrLn "[WARNING] no protocol module specified in [info]plugins")
        protocols -> mapM_ handleExport protocols
    mapM_ handleExport [export | export@(ProtocolExport {}) <- exports]
    name' <- getConfig "info" "name"
    desc <- getConfig "info" "description"
    pass <- getConfig "remote" "password"
    doAction (RegistrationAction name' desc (Just pass))
    loop

loop :: Arata ()
loop = forever $ recv >>= \case
    Nothing   -> return ()
    Just line -> gets fromProtocol >>= ($ parseMessage line) >>= mapM handleEvent >>= mapM_ doAction . concat

handleEvent :: Event -> Arata [Action]
handleEvent event = do
    liftIO (putStrLn ("-> " ++ show event))
    handleEvent' event

handleEvent' :: Event -> Arata [Action]
handleEvent' (RegistrationEvent _) = burst
handleEvent' (PingEvent str) = return [PongAction str]
handleEvent' (IntroductionEvent uid' nick' user' name' ip' host' vHost' uModes acc mTs) = do
    ts' <- liftIO (fmap round getPOSIXTime)
    addClient Client
        { uid       = uid'
        , nick      = nick'
        , ts        = fromMaybe ts' mTs
        , user      = user'
        , realName  = name'
        , userModes = uModes
        , vHost     = vHost'
        , host      = host'
        , ip        = ip'
        , cert      = Nothing
        , account   = acc
        }
    return []
handleEvent' (CertEvent src crt) = getClient src >>= \case
    Nothing  -> fail "[FATAL] desynchronization"
    Just cli -> addClient cli { cert = Just crt } >> return []
handleEvent' (NickEvent src new mTs) = getClient src >>= \case
    Nothing  -> fail "[FATAL] desynchronization"
    Just cli -> do
        ts' <- liftIO (fmap round getPOSIXTime)
        addClient cli { nick = new, ts = fromMaybe ts' mTs } >> return []
handleEvent' (PrivmsgEvent src dst msg) = case words msg of
    []     -> return []
    (x:xs) -> getServ dst >>= \case
        Nothing  -> return []
        Just srv -> getCommand srv x >>= \case
            Nothing  -> return [NoticeAction dst src ("Invalid command. Use \2/msg TODO HELP\2 for a list of valid commands.")]
            Just cmd -> commandH cmd src dst xs
handleEvent' _ = return []

doAction :: Action -> Arata ()
doAction action = do
    doAction' action
    gets toProtocol >>= ($ action) >>= mapM_ (send . show)
    liftIO (putStrLn ("<- " ++ show action))

doAction' :: Action -> Arata ()
doAction' (IntroductionAction uid' nick' user' name' host' acc ts') = addClient Client
        { uid       = uid'
        , nick      = nick'
        , ts        = ts'
        , user      = user'
        , realName  = name'
        , userModes = []
        , vHost     = "127.0.0.1"
        , host      = host'
        , ip        = "127.0.0.1"
        , cert      = Nothing
        , account   = acc
        }
doAction' (AuthAction src acc) = getClient src >>= \case
    Nothing  -> fail "[FATAL] desynchronization"
    Just cli -> addClient cli { account = acc }
doAction' _ = return ()

burst :: Arata [Action]
burst = gets pluginExports >>= liftM concat . mapM handleExport

handleExport :: PluginExport -> Arata [Action]
handleExport (ProtocolExport from to mkUid) = do
    setFromProtocol from
    setToProtocol to
    setMakeUid mkUid
    return []
handleExport (ServExport name') = do
    serverName <- getConfig "info" "name"
    sect <- getSection name'
    n <- gets nextUid
    uid' <- gets makeUid >>= ($ n)
    modify (\env -> env { nextUid = succ n })
    addServ uid' name'
    return [IntroductionAction uid' (sect "nick") (sect "user") (sect "name") serverName Nothing 1]
handleExport (CommandExport name' c) = addCommand name' c >> return []
