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

module Arata.NickServ.Login where

import Data.Maybe
import Data.IxSet as Ix
import Dated
import Arata.Types
import Arata.DB
import Arata.Protocol.Charybdis

exports = [ CommandExport "nickserv" cmd
          , CommandExport "nickserv" (Alias "IDENTIFY" "LOGIN")
          , CommandExport "nickserv" (Alias "ID" "LOGIN")
          ]

cmd :: Command
cmd = (defaultCommand "LOGIN" handler)
    { short     = "Logs into an account"
    , long      = "TODO"
    }

handler :: CommandH
handler src dst [] = nsLogin src dst Nothing Nothing >>= mapM_ (protoNotice dst src) . snd
handler src dst [pass] = nsLogin src dst Nothing (Just pass) >>= mapM_ (protoNotice dst src) . snd
handler src dst (accName:pass:_) = nsLogin src dst (Just accName) (Just pass) >>= mapM_ (protoNotice dst src) . snd

nsLogin :: Client -> Client -> Maybe String -> Maybe String -> Arata (Bool, [String])
nsLogin src _ Nothing Nothing
    | isJust (account src) = return (False, ["You are already logged in as \2" ++ fromJust (account src) ++ "\2."])
    | isNothing (cert src) = return (False, ["Failed to login to \2" ++ nick src ++ "\2."])
    | otherwise = do
        accs <- queryDB $ QueryAccountsByNick (nick src)
        if Ix.null accs
            then return (False, ['\2' : nick src ++ "\2 is not registered."])
            else if f (auths (fromJust (getOne accs)))
                then do
                    protoAuthClient src (Just (nick src))
                    return (True, ["You are now logged in as \2" ++ nick src ++ "\2."])
                else return (False, ["Failed to login to \2" ++ nick src ++ "\2."])
  where f (CertAuth cert' :@ _ : xs)
            | fromJust (cert src) == cert' = True
            | otherwise = f xs
        f [] = False
        f (_:xs) = f xs
nsLogin src dst Nothing pass = nsLogin src dst (Just (nick src)) pass
nsLogin src _ (Just accName) (Just pass)
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
        f [] = False
        f (_:xs) = f xs
nsLogin _ _ _ _ = fail "Something went wrong"
