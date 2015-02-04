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

module NickServ.Login where

import Data.Maybe
import Data.IxSet as Ix
import qualified Data.Map as M
import Control.Monad (liftM)
import Dated
import Arata.Types
import Arata.Helper
import Arata.DB
import Ext.Help

exports = [ CommandExport "nickserv" cmd
          , CommandExport "nickserv" (Alias "IDENTIFY" "LOGIN")
          , CommandExport "nickserv" (Alias "ID" "LOGIN")
          ]

cmd = (defaultCommand "LOGIN" handler) { extensions = M.singleton (typeOf extHelp) (toDyn extHelp)}

extHelp = defaultExtHelp { short = "Logs into an account" }

handler src dst []           = nsLogin src dst Nothing Nothing
handler src dst [pass]       = nsLogin src dst Nothing (Just pass)
handler src dst (acc:pass:_) = nsLogin src dst (Just acc) (Just pass)

nsLogin :: String -> String -> Maybe String -> Maybe String -> Arata [Action]
nsLogin src dst mAccName mPass = getClient src >>= \case
    Nothing   -> fail "[FATAL] desynchronization"
    Just (Client { account = Just accName' }) -> return [NoticeAction dst src ("You are already logged in as \2" ++ accName' ++ "\2.")]
    Just cli  -> let accName' = fromMaybe (nick cli) mAccName in liftM getOne (queryDB (QueryAccountsByNick accName')) >>= \case
        Nothing  -> return [NoticeAction dst src ('\2' : accName' ++ "\2 is not registered.")]
        Just acc -> return $ if canAuth cli (auths acc)
            then [AuthAction src (Just (accName acc)), NoticeAction dst src ("You are now logged in as \2" ++ accName acc ++ "\2.")]
            else [NoticeAction dst src ("Failed to login to \2" ++ accName' ++ "\2.")]
  where canAuth cli (CertAuth crt :@ _ : xs) = case cert cli of
            Nothing -> canAuth cli xs
            Just c  -> if c == crt then True else canAuth cli xs
        canAuth cli (PassAuth passwd :@ _ : xs) = case mPass of
            Nothing -> canAuth cli xs
            Just p  -> if p == passwd then True else canAuth cli xs
        canAuth _ [] = False
