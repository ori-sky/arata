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

type H = Client -> Client -> String -> String -> Arata ()

protoIntroduceClient :: (Integral a, Show a) => a -> String -> String -> String -> String -> Maybe String -> Maybe H -> Arata Client
protoIntroduceClient id' nick' user' name host' acc _ = do
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
            }
        line = printf ":%s EUID %s 1 1 +Sio %s %s 127.0.0.1 %s 127.0.0.1 %s :%s" sid nick' user' host' uid' (fromMaybe "*" acc) name
    send line
    return client

protoDisconnect :: String -> Arata ()
protoDisconnect reason = send ("SQUIT " ++ "0AR" ++ " :" ++ reason)

protoHandleMessage :: Message -> Arata ()
protoHandleMessage (Message _ _ "PASS" (pass:"TS":"6":_)) = do
    password <- getConfig "local" "password"
    unless (pass == password) $ do
        protoDisconnect "Invalid password"
        fail "Invalid password"
protoHandleMessage (Message _ _ "SERVER" _) = do
    ts <- liftIO (fmap round getPOSIXTime)
    send ("SVINFO 6 6 0 :" ++ show ts)
    gets burst >>= id
protoHandleMessage m@(Message _ _ "PRIVMSG" (uid:_)) = do
    sid <- getConfig "info" "id"
    if uid == sid ++ "A00001" then handleCS m else return ()
protoHandleMessage _ = return ()

protoPrivmsg :: Client -> Client -> String -> Arata ()
protoPrivmsg src dst msg = send (show (uid src) ++ " PRIVMSG " ++ show (uid dst) ++ " :" ++ msg)

handleCS :: Message -> Arata ()
handleCS (Message _ prefix _ (_:cmd:xs)) = do
    sid <- getConfig "info" "id"
    send (':' : sid ++ "A00001 NOTICE " ++ show (fromMaybe NoPrefix prefix) ++ " :Invalid command. Use \x02/msg ChanServ HELP\x02 for a list of valid commands.")
handleCS _ = return ()
