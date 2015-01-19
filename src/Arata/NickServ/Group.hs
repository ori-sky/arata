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

module Arata.NickServ.Group where

import Arata.Types
import Arata.Protocol.Charybdis

exports = [CommandExport "nickserv" cmd]

cmd :: Command
cmd = (defaultCommand "GROUP" handler)
    { short     = "Adds your current nick to your account"
    , long      = "TODO"
    }

handler :: CommandH
handler src dst _ = protoNotice dst src "TODO"
