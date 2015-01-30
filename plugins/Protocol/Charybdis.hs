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

module Protocol.Charybdis where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Text.Printf (printf)
import Arata.Types
import Arata.Helper (getConfig, getClientByNick)

exports = [ProtocolExport from to mkUid]

from (Message _ _ "PING"   (server1:_))       = return [PingEvent server1]
from (Message _ _ "PASS"   (pass:"TS":"6":_)) = return [PassEvent pass]
from (Message _ _ "SERVER" (name':"1":_))     = return [RegistrationEvent name']
from (Message _ _ "EUID"   (nick:_:ts:('+':uModes):user:vHost:ip:uid:host:acc:name:_)) = return [IntroductionEvent uid nick user name ip host vHost uModes mAcc (Just (read ts))]
  where mAcc = if acc == "*" then Nothing else Just acc
from (Message _ (Just (StringPrefix src)) "NICK" (newNick:newTs:_)) = return [NickEvent src newNick (Just (read newTs))]
from (Message _ (Just (StringPrefix src)) "PRIVMSG" (dst:msg:_))
    | xs == ""  = return [PrivmsgEvent src dst msg]
    | otherwise = getClientByNick dstNick >>= \case
        Nothing  -> return []
        Just cli -> return [PrivmsgEvent src (uid cli) msg]
  where (dstNick, xs) = (break (== '@') dst)
from (Message _ (Just (StringPrefix src)) "ENCAP" (_:"CERTFP":cert':xs)) = return [CertEvent src cert']
from _ = return []

to (RegistrationAction name desc pass) = do
    sid <- getConfig "info" "id"
    return [ Message () Nothing "PASS" [fromMaybe "*" pass, "TS", "6", sid]
           , Message () Nothing "CAPAB" ["ENCAP QS EX IE EUID SAVE TB"]
           , Message () Nothing "SERVER" [name, "1", desc]
           ]
to (QuitAction sid reason) = return [Message () Nothing "SQUIT" [sid, reason]]
to (PongAction str)        = return [Message () Nothing "PONG" [str]]
to (IntroductionAction uid nick user name host acc ts) = do
    sid <- getConfig "info" "id"
    return [Message () (Just (StringPrefix sid)) "EUID" [nick, "1", show ts, "+Sio", user, host, "127.0.0.1", uid, "127.0.0.1", fromMaybe "*" acc, name]]
to (AuthAction uid acc)        = encap Nothing "SU" [uid, fromMaybe "*" acc] >>= return . return -- return Message into [] and then into Arata
to (PrivmsgAction src dst msg) = return [Message () (Just (StringPrefix src)) "PRIVMSG" [dst, msg]]
to (NoticeAction src dst msg)  = return [Message () (Just (StringPrefix src)) "NOTICE" [dst, msg]]

encap :: Maybe String -> String -> [String] -> Arata Message
encap dst cmd xs = do
    sid <- getConfig "info" "id"
    return (Message () (Just (StringPrefix sid)) "ENCAP" (fromMaybe "*" dst : cmd : xs))

mkUid x = liftM (++ mkId x) (getConfig "info" "id")

mkId :: Integer -> String
mkId x = 'A' : printf "%05s" (showIntAtBase 36 toChr x "")
  where toChr c
            | 0 <= c && c <= 9 = intToDigit c
            | otherwise = toEnum (c + (65 - 10))
