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

module NickServ.Add where

import Data.Maybe
import Data.Char (toUpper)
import Data.IxSet as Ix
import Data.Time.Clock
import Control.Monad.State (liftIO)
import Dated
import Arata.Types
import Arata.Helper
import Arata.DB
import Arata.Protocol.Charybdis

exports = [CommandExport "nickserv" cmd]

cmd :: Command
cmd = (defaultCommand "ADD" handler)
    { short     = "Adds a property to your account"
    , long      = "TODO"
    }

handler :: CommandH
handler src dst [] = do
    nick' <- getConfig "nickserv" "nick"
    protoNotice dst src "Not enough parameters for \2ADD\2."
    protoNotice dst src "Syntax: ADD <option> <parameters>"
    protoNotice dst src ("For more information, type \2/msg " ++ nick' ++ " HELP ADD")
handler src dst (x:xs) = nsAdd src dst (map toUpper x : xs)

nsAdd :: PrivmsgH
nsAdd src dst ["PASSWORD"] = do
    protoNotice dst src "Not enough parameters for \2ADD PASSWORD\2."
    protoNotice dst src "Syntax: ADD PASSWORD <password>"
nsAdd src dst ("PASSWORD":pass:_) = do
    (succeeded, notices) <- nsAddAuth src dst (PassAuth pass)
    if succeeded
        then protoNotice dst src ("The password \2" ++ pass ++ "\2 has been added to your account.")
        else mapM_ (protoNotice dst src) notices

nsAdd src dst ("MYCERT":_) = case cert src of
    Nothing -> protoNotice dst src "You are not connected with an SSL certificate."
    Just cert' -> do
        (succeeded, notices) <- nsAddAuth src dst (CertAuth cert')
        if succeeded
            then protoNotice dst src ("The SSL certificate \2" ++ cert' ++ "\2 has been added to your account.")
            else mapM_ (protoNotice dst src) notices

nsAdd src dst _ = do
    nick' <- getConfig "nickserv" "nick"
    protoNotice dst src ("Invalid option for \2ADD\2. Use \2/msg " ++ nick' ++ " HELP ADD\2 for a list of valid options.")

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
