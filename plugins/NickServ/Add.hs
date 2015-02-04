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

module NickServ.Add where

import Data.Maybe
import Data.Char (toUpper)
import Data.IxSet as Ix
import Data.Time.Clock
import qualified Data.Map as M
import Control.Monad.State (liftIO)
import Dated
import Arata.Types
import Arata.Helper
import Arata.DB
import Ext.Help

exports = [CommandExport "nickserv" cmd]

cmd = (defaultCommand "ADD" handler) { extensions = M.singleton (typeOf extHelp) (toDyn extHelp)}

extHelp = defaultExtHelp { short = "Adds a property to your account" }

handler src dst [] = do
    nick <- getConfig "nickserv" "nick"
    return [ NoticeAction dst src "Not enough parameters for \2ADD\2."
           , NoticeAction dst src "Syntax: ADD <option> <parameters>"
           , NoticeAction dst src ("For more information, type \2/msg " ++ nick ++ " HELP ADD\2")
           ]
handler src dst (x:xs) = nsAdd src dst (map toUpper x : xs)

nsAdd :: CommandH
nsAdd src dst ["PASSWORD"] = return [ NoticeAction dst src "Not enough parameters for \2ADD PASSWORD\2."
                                    , NoticeAction dst src "Syntax: ADD PASSWORD <password>"
                                    ]
nsAdd src dst ("PASSWORD":pass:_) = do
    (succeeded, notices) <- addAuth src (PassAuth pass)
    return $ if succeeded
        then [NoticeAction dst src ("The password \2" ++ pass ++ "\2 has been added to your account.")]
        else map (NoticeAction dst src) notices

nsAdd src dst ("MYCERT":_) = getClient src >>= \case
    Nothing                             -> fail "[FATAL] desynchronization"
    Just (Client { cert = Nothing })    -> return [NoticeAction dst src "You are not connected with an SSL certificate."]
    Just (Client { cert = Just cert' }) -> do
        (succeeded, notices) <- addAuth src (CertAuth cert')
        return $ if succeeded
            then [NoticeAction dst src ("The SSL certificate \2" ++ cert' ++ "\2 has been added to your account.")]
            else map (NoticeAction dst src) notices

nsAdd src dst _ = do
    nick' <- getConfig "nickserv" "nick"
    return [NoticeAction dst src ("Invalid option for \2ADD\2. Use \2/msg " ++ nick' ++ " HELP ADD\2 for a list of valid options.")]

addAuth :: String -> AuthMethod -> Arata (Bool, [String])
addAuth src auth = getClient src >>= \case
    Nothing                                  -> fail "[FATAL] desynchronization"
    Just (Client { account = Nothing })      -> return (False, ["You are not logged in."])
    Just (Client { account = Just accName }) -> do
        accs <- queryDB (QueryAccountsByName accName)
        case getOne accs of
            Nothing  -> return (False, ["You are not logged in. Please report this as a bug."])
            Just acc -> do
                t <- liftIO getCurrentTime
                updateDB $ UpdateAccount (acc { auths = (auth :@ t) : auths acc })
                return (True, ["The authentication method has been added to your account."])
