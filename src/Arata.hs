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
import Data.Time.Clock
import Data.ConfigFile.Monadic
import Data.IxSet as Ix
import Data.Acid
import Control.Monad.State
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import System.IO.Error
import Network.Connection
import Dated
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
    protoNotice dst src ("Invalid command. Use \2/msg " ++ nick' ++ " HELP\2 for a list of valid commands.")

nsHandler' :: PrivmsgH

nsHandler' src dst ["REGISTER"] = nsRegister src dst Nothing Nothing >>= mapM_ (protoNotice dst src) . snd
nsHandler' src dst ["REGISTER", pass] = nsRegister src dst (Just pass) Nothing >>= mapM_ (protoNotice dst src) . snd
nsHandler' src dst ("REGISTER":pass:email:_)
    | '@' `elem` email = nsRegister src dst (Just pass) (Just email) >>= mapM_ (protoNotice dst src) . snd
    | otherwise = protoNotice dst src ('\2' : email ++ "\2 is not a valid email address.")

nsHandler' src dst ["LOGIN"] = nsLogin src dst Nothing Nothing >>= mapM_ (protoNotice dst src) . snd
nsHandler' src dst ["LOGIN", pass] = nsLogin src dst Nothing (Just pass) >>= mapM_ (protoNotice dst src) . snd
nsHandler' src dst ("LOGIN":accName:pass:_) = nsLogin src dst (Just accName) (Just pass) >>= mapM_ (protoNotice dst src) . snd

nsHandler' src dst ["ADD"] = do
    protoNotice dst src "Not enough parameters for \2ADD\2."
    protoNotice dst src "Syntax: ADD <option> <parameters>"
nsHandler' src dst ("ADD":x:xs) = nsAdd src dst (map toUpper x : xs)

nsHandler' src dst _ = do
    nick' <- getConfig "nickserv" "nick"
    protoNotice dst src ("Invalid command. Use \2/msg " ++ nick' ++ " HELP\2 for a list of valid commands.")

-- stage 3 handlers

nsAdd :: PrivmsgH
nsAdd src dst ("PASSWORD":pass:_) = do
    (succeeded, notices) <- nsAddAuth src dst (PassAuth pass)
    if succeeded
        then protoNotice dst src ("The password \2" ++ pass ++ "\2 has been added to your account.")
        else mapM_ (protoNotice dst src) notices
nsAdd src dst ["PASSWORD"] = do
    protoNotice dst src "Not enough parameters for \2ADD PASSWORD\2."
    protoNotice dst src "Syntax: ADD PASSWORD <password>"
nsAdd src dst _ = do
    nick' <- getConfig "nickserv" "nick"
    protoNotice dst src ("Invalid option for \2ADD\2. Use \2/msg " ++ nick' ++ " HELP ADD\2 for a list of valid options.")

-- nickserv functions

nsRegister :: Client -> Client -> Maybe String -> Maybe String -> Arata (Bool, [String])
nsRegister src _ pass email = do
    accs <- queryDB $ QueryAccountsByNick (nick src)
    if Ix.null accs
        then do
            updateDB $ AddAccount (Account 1 (nick src) [] [])
            protoAuthClient src (Just (nick src))
            return (True, [msg])
        else return (False, ['\2' : nick src ++ "\2 is already registered."])
  where msgEmail = " to \2" ++ (fromJust email) ++ "\2"
        msgPass = " with the password \2" ++ (fromJust pass) ++ "\2"
        msg = '\2' : nick src ++ "\2 is now registered"
            ++ (if isJust email then msgEmail else "")
            ++ (if isJust pass then msgPass else "")
            ++ "."

nsLogin :: Client -> Client -> Maybe String -> Maybe String -> Arata (Bool, [String])
nsLogin src dst Nothing Nothing
    | isJust (account src) = return (False, ["You are already logged in as \2" ++ fromJust (account src) ++ "\2."])
    | otherwise = return (False, ["Failed to login to \2" ++ nick src ++ "\2."])
nsLogin src dst Nothing pass = nsLogin src dst (Just (nick src)) pass
nsLogin src dst (Just accName) (Just pass)
    | isJust (account src) = return (False, ["You are already logged in as \2" ++ fromJust (account src) ++ "\2."])
    | otherwise = do
        accs <- queryDB $ QueryAccountsByNick (nick src)
        if Ix.null accs
            then return (False, ['\2' : accName ++ "\2 is not registered."])
            else if f (auths (fromJust (getOne accs)))
                then do
                    protoAuthClient src (Just accName)
                    return (True, ["You are now logged in as \2" ++ accName ++ "\2."])
                else return (False, ["Failed to login to \2" ++ accName ++ "\2."])
  where f (PassAuth p :@ _ : xs)
            | pass == p = True
            | otherwise = f xs
        f _ = False
nsLogin _ _ _ _ = fail "Something went wrong"

nsAddAuth :: Client -> Client -> AuthMethod -> Arata (Bool, [String])
nsAddAuth src _ auth
    | account src == Nothing = return (False, ["You are not logged in."])
    | otherwise = do
        accs <- queryDB $ QueryAccountsByName (fromJust (account src))
        case getOne accs of
            Nothing  -> fail "[FATAL] client account not found in database"
            Just acc -> do
                t <- liftIO getCurrentTime
                updateDB $ UpdateAccount (acc { auths = (auth :@ t) : auths acc })
                return (True, ["The authentication method has been added to your account."])
