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

module NickServ.Logout where

import Arata.Types
import Arata.Helper (getClient)

exports = [CommandExport "nickserv" cmd]

cmd :: Command
cmd = (defaultCommand "LOGOUT" handler)
    { short     = "Logs out of your account"
    , long      = "TODO"
    }

handler src dst _ = getClient src >>= \case
    Nothing -> fail "[FATAL] desynchronization"
    Just (Client { account = Nothing }) -> return [NoticeAction dst src "You are not logged in."]
    Just _ -> return [AuthAction src Nothing, NoticeAction dst src "You have been logged out."]
