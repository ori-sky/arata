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

module Arata.Protocol.Charybdis where

import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX
import Control.Monad.State
import Text.Printf
import Arata.Types
import Arata.TS6
import Arata.Helper

protoGetUid :: (Integral a, Show a) => String -> a -> String
protoGetUid sid = (sid ++) . intToUid

protoRegister :: Arata ()
protoRegister = do
    sid <- getConfig "info" "id"
    name <- getConfig "info" "name"
    desc <- getConfig "info" "description"
    password <- getConfig "remote" "password"
    send ("PASS " ++ password ++ " TS 6 :" ++ sid)
    send "CAPAB :ENCAP QS EX IE EUID SAVE TB"
    send ("SERVER " ++ name ++ " 1 :" ++ desc)

protoIntroduceClient :: (Integral a, Show a) => a -> String -> String -> String -> String -> Maybe String -> Maybe PrivmsgH -> Arata Client
protoIntroduceClient id' nick' user' name host' acc f = do
    sid <- getConfig "info" "id"
    let uid' = protoGetUid sid id'
        client = Client
            { uid       = uid'
            , nick      = nick'
            , ts        = 1
            , user      = user'
            , realName  = name
            , userModes = "Sio"
            , vHost     = host'
            , host      = "127.0.0.1"
            , ip        = "127.0.0.1"
            , account   = acc
            , privmsgH  = f
            }
        line = printf ":%s EUID %s 1 1 +Sio %s %s 127.0.0.1 %s 127.0.0.1 %s :%s" sid nick' user' host' uid' (fromMaybe "*" acc) name
    send line
    addClient client
    return client

protoAuthClient :: Client -> Maybe String -> Arata ()
protoAuthClient cli acc = do
    addClient cli { account = acc }
    sid <- getConfig "info" "id"
    encap sid Nothing "SU" [uid cli, fromMaybe "*" acc]

encap :: String -> Maybe String -> String -> [String] -> Arata ()
encap src dst cmd params = send (':' : src ++ " ENCAP " ++ fromMaybe "*" dst ++ ' ' : cmd ++ paramString)
  where paramString = case params of
            []     -> ""
            (x:[]) -> " :" ++ x
            _      -> ' ' : unwords (init params) ++ " :" ++ last params

protoDisconnect :: String -> Arata ()
protoDisconnect reason = send ("SQUIT " ++ "0AR" ++ " :" ++ reason)

protoHandleMessage :: Message -> Arata ()
protoHandleMessage (Message _ _ "PING" (server1:_)) = send ("PONG :" ++ server1)
protoHandleMessage (Message _ _ "PASS" (pass:"TS":"6":_)) = do
    password <- getConfig "local" "password"
    unless (pass == password) $ do
        protoDisconnect "Invalid password"
        fail "Invalid password"
protoHandleMessage (Message _ _ "SERVER" _) = do
    ts <- liftIO (fmap round getPOSIXTime)
    send ("SVINFO 6 6 0 :" ++ show ts)
    gets burst >>= id
protoHandleMessage (Message _ _ "EUID" (nick':_:ts':('+':umodes):user':vHost':ip':uid':host':acc:name:_)) = do
    addClient client
  where client = Client
            { uid       = uid'
            , nick      = nick'
            , ts        = read ts'
            , user      = user'
            , realName  = name
            , userModes = umodes
            , vHost     = vHost'
            , host      = host'
            , ip        = ip'
            , account   = if acc == "*" then Nothing else Just acc
            , privmsgH  = Nothing
            }
protoHandleMessage (Message _ (Just (StringPrefix srcUid)) "NICK" (newNick:newTs:_)) = do
    Just src <- getClient srcUid
    addClient src { nick = newNick, ts = read newTs }
protoHandleMessage (Message _ (Just (StringPrefix srcUid)) "PRIVMSG" (dstUid:msg:_)) = do
    Just src <- getClient srcUid
    getClient dstUid >>= \case
        Nothing  -> return ()
        Just dst -> case privmsgH dst of
            Nothing -> return ()
            Just f  -> f src dst (words msg)
protoHandleMessage _ = return ()

protoPrivmsg :: Client -> Client -> String -> Arata ()
protoPrivmsg src dst msg = send (':' : uid src ++ " PRIVMSG " ++ uid dst ++ " :" ++ msg)

protoNotice :: Client -> Client -> String -> Arata ()
protoNotice src dst msg = send (':' : uid src ++ " NOTICE " ++ uid dst ++ " :" ++ msg)
