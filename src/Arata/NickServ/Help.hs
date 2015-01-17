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

module Arata.NickServ.Help where

import Data.Char (toUpper)
import qualified Data.Map as M
import Control.Monad (forM_)
import Align
import Arata.Types
import Arata.Helper
import Arata.Protocol.Charybdis

exports = [CommandExport "nickserv" cmd]

cmd :: Command
cmd = (defaultCommand "HELP" handler)
    { short     = "Displays help information"
    , long      = "\2NICK\2 allows users to register a nickname and prevent others from using that nick. \2NICK\2 allows the owner of a nick to disconnect a user that is using their nick."
    }

handler :: CommandH
handler src dst [] = handler' src dst ["HELP"]
handler src dst xs = handler' src dst (map (map toUpper) xs)

handler' :: CommandH
handler' src dst ("HELP":_) = do
    nick' <- getConfig "nickserv" "nick"
    mapM_ (protoNotice dst src) (long cmd$:$ 60)
    protoNotice dst src " "
    protoNotice dst src "For more information about a command, type:"
    protoNotice dst src ("    \2/msg " ++ nick' ++ " HELP <command>\2")
    protoNotice dst src " "
    protoNotice dst src "The following commands are available:"
    getCommands "nickserv" >>= \case
        Nothing   -> return ()
        Just cmds -> do
            forM_ (M.elems cmds) $ \cmd -> protoNotice dst src ('\2' : name cmd ++ '\2' : replicate (n + 3 - length (name cmd)) ' ' ++ short cmd)
          where n = maximum (map length (M.keys cmds))
    --protoNotice dst src "\2ADD\2        Adds a property to your account"
    --protoNotice dst src "\2CONFIRM\2    Confirms a previous command"
    --protoNotice dst src "\2DEL\2        Removes a property from your account"
    --protoNotice dst src "\2DROP\2       Drops your account"
    --protoNotice dst src "\2GROUP\2      Adds a nick to your account"
    --protoNotice dst src "\2HELP\2       Displays help information"
    --protoNotice dst src "\2INFO\2       Displays account information"
    --protoNotice dst src "\2LOGIN\2      Logs into an account"
    --protoNotice dst src "\2LOGOUT\2     Logs out of your account"
    --protoNotice dst src "\2NICK\2       Recovers a nick and changes your nick to it"
    --protoNotice dst src "\2RECOVER\2    Recovers a nick grouped to your account"
    --protoNotice dst src "\2REGISTER\2   Registers a new account"
    --protoNotice dst src "\2SHOW\2       Shows account properties"
    --protoNotice dst src "\2UNGROUP\2    Removes a nick from your account"
    protoNotice dst src " "
    protoNotice dst src "   End of HELP"
handler' src dst (x:_) = protoNotice dst src ("No help information available for \2" ++ x ++ "\2.")
handler' src dst [] = protoNotice dst src "No help information available."
