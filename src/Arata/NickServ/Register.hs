{- Copyright 2015 David Farrell <shokku.ra@gmail.com>

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

module Arata.NickServ.Register where

import Data.Maybe
import Data.IxSet as Ix
import Arata.Types
import Arata.Helper
import Arata.DB
import Arata.Protocol.Charybdis

exports = [CommandExport "nickserv" cmd]

cmd :: Command
cmd = (defaultCommand "REGISTER" handler)
    { short     = "Registers a new account"
    , long      = "TODO"
    }

handler :: CommandH
handler src dst [] = nsRegister src dst Nothing Nothing >>= mapM_ (protoNotice dst src) . snd
handler src dst [pass] = nsRegister src dst (Just pass) Nothing >>= mapM_ (protoNotice dst src) . snd
handler src dst (pass:email:_)
    | '@' `elem` email = nsRegister src dst (Just pass) (Just email) >>= mapM_ (protoNotice dst src) . snd
    | otherwise = protoNotice dst src ('\2' : email ++ "\2 is not a valid email address.")

nsRegister :: Client -> Client -> Maybe String -> Maybe String -> Arata (Bool, [String])
nsRegister src _ pass email = do
    accs <- queryDB $ QueryAccountsByNick (nick src)
    if Ix.null accs
        then do
            accId' <- firstAvailableID
            updateDB $ AddAccount (Account accId' (nick src) [] [])
            protoAuthClient src (Just (nick src))
            return (True, [msg])
        else return (False, ['\2' : nick src ++ "\2 is already registered."])
  where msgEmail = " to \2" ++ (fromJust email) ++ "\2"
        msgPass = " with the password \2" ++ (fromJust pass) ++ "\2"
        msg = '\2' : nick src ++ "\2 is now registered"
            ++ (if isJust email then msgEmail else "")
            ++ (if isJust pass then msgPass else "")
            ++ "."
