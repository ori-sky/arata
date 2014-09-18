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

import Data.Maybe (isJust, fromJust)
import Data.Char (toUpper)
import Data.ConfigFile.Monadic
import Data.IxSet as Ix
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
import Arata.DB
import Arata.Protocol.Charybdis

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
    cs <- getSection "chanserv"
    ns <- getSection "nickserv"
    protoIntroduceClient 1 (cs "nick") (cs "user") (cs "name") (cs "host") Nothing (Just csHandler)
    protoIntroduceClient 2 (ns "nick") (ns "user") (ns "name") (ns "host") Nothing (Just nsHandler)
    return ()

-- stage 1 handlers

csHandler :: PrivmsgH
csHandler src dst (x:xs) = csHandler' src dst (map toUpper x : xs)
csHandler src dst [] = csHandler' src dst []

nsHandler :: PrivmsgH
nsHandler src dst (x:xs) = nsHandler' src dst (map toUpper x : xs)
nsHandler src dst [] = nsHandler' src dst []

-- stage 2 handlers

csHandler' :: PrivmsgH
csHandler' src dst ("HELP":_) = protoNotice dst src "Not implemented"
csHandler' src dst _ = do
    nick' <- getConfig "chanserv" "nick"
    protoNotice dst src ("Invalid command. Use \x02/msg " ++ nick' ++ " HELP\x02 for a list of valid commands.")

nsHandler' :: PrivmsgH
nsHandler' src dst ("REGISTER":pass:email:_)
    | '@' `elem` email = nsRegister src dst (Just pass) (Just email)
    | otherwise = protoNotice dst src ('\x02' : email ++ "\x02 is not a valid email address.")
nsHandler' src dst ("REGISTER":pass:[]) = nsRegister src dst (Just pass) Nothing
nsHandler' src dst ("REGISTER":[]) = nsRegister src dst Nothing Nothing
--nsHandler' src dst ("ADD":"PASSWORD":pass:_) = nsAddAuth src dst (PassAuth pass)
nsHandler' src dst _ = do
    nick' <- getConfig "nickserv" "nick"
    protoNotice dst src ("Invalid command. Use \x02/msg " ++ nick' ++ " HELP\x02 for a list of valid commands.")

-- nickserv functions

nsRegister :: Client -> Client -> Maybe String -> Maybe String -> Arata ()
nsRegister src dst pass email = do
    accs <- queryDB (QueryAccountsByNick (nick src))
    if Ix.null accs
        then do
            updateDB (AddAccount (Account 1 (nick src) [] []))
            protoNotice dst src msg
            protoAuthClient src (Just (nick src))
        else protoNotice dst src ('\x02' : nick src ++ "\x02 is already registered.")
  where msgEmail = " to \x02" ++ (fromJust email) ++ "\x02"
        msgPass = " with the password \x02" ++ (fromJust pass) ++ "\x02"
        msg = '\x02' : nick src ++ "\x02 is now registered"
            ++ (if isJust email then msgEmail else "")
            ++ (if isJust pass then msgPass else "")
            ++ "."

--nsAddAuth :: Client -> Client -> AuthMethod -> Arata ()
--nsAddAuth src dst auth =
