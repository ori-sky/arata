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

{-# LANGUAGE LambdaCase #-}

module NickServ.Register where

import Data.Maybe
import Data.IxSet as Ix
import qualified Data.Map as M
import Arata.Types
import Arata.Helper
import Arata.DB
import Ext.Help

import qualified Data.Map as M
import Arata.Types
import Ext.Help

exports = [CommandExport "nickserv" cmd]

cmd = (defaultCommand "REGISTER" handler) { extensions = M.singleton (typeOf extHelp) (toDyn extHelp)}

extHelp = defaultExtHelp { short = "Registers a new account" }

handler src dst [] = nsRegister src dst Nothing Nothing
handler src dst [pass] = nsRegister src dst (Just pass) Nothing
handler src dst (pass:email:_)
    | '@' `elem` email = nsRegister src dst (Just pass) (Just email)
    | otherwise = return [NoticeAction dst src ('\2' : email ++ "\2 is not a valid email address.")]

nsRegister :: String -> String -> Maybe String -> Maybe String -> Arata [Action]
nsRegister src dst pass email = getClient src >>= \case
    Nothing -> fail "[FATAL] desynchronization"
    Just (Client { account = Just accName' }) -> return [NoticeAction dst src ("You are already logged in as \2" ++ accName' ++ "\2.")]
    Just cli -> do
        let msgEmail = " to \2" ++ (fromJust email) ++ "\2"
            msgPass = " with the password \2" ++ (fromJust pass) ++ "\2"
            msg = '\2' : nick cli ++ "\2 is now registered"
                ++ (if isJust email then msgEmail else "")
                ++ (if isJust pass then msgPass else "")
                ++ "."
        accs <- queryDB (QueryAccountsByNick (nick cli))
        if Ix.null accs
            then do
                accId' <- firstAvailableID
                updateDB $ AddAccount (Account accId' (nick cli) [] [])
                return [AuthAction src (Just (nick cli)), NoticeAction dst src msg]
            else return [NoticeAction dst src ('\2' : nick cli ++ "\2 is already registered.")]
