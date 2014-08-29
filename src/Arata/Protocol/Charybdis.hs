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
import Text.Printf
import Arata.Types
import Arata.TS6
import Arata.Helper

protoRegister :: Arata ()
protoRegister = do
    send ("PASS " ++ "password" ++ " TS 6 :" ++ "0AR")
    send "CAPAB :ENCAP QS EX IE EUID SAVE TB"
    send "SERVER services.int 1 :Arata IRC Services"

protoIntroduceClient :: Client -> Arata ()
protoIntroduceClient client = send line
  where line = printf ":%s EUID %s 1 %u +%s %s %s %s %s %s %s :%s"
            "0AR" (nick client) (ts client) (userModes client) (user client) (vHost client) (host client)
            ("0AR" ++ intToUid (uid client)) (ip client) (fromMaybe "*" (account client)) (realName client)

protoDisconnect :: String -> Arata ()
protoDisconnect reason = send ("SQUIT " ++ "0AR" ++ " :" ++ reason)
