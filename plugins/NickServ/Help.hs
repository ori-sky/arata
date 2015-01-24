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

module NickServ.Help where

import Data.Maybe (mapMaybe)
import Data.Char (toUpper)
import qualified Data.Map as M
import Control.Monad (forM_)
import Align
import Arata.Types
import Arata.Helper
import Arata.Protocol.Charybdis

exports = [ CommandExport "nickserv" cmdHelp
          , CommandExport "nickserv" (Alias "H" "HELP")
          ]

cmdHelp :: Command
cmdHelp = (defaultCommand "HELP" handler)
    { short         = "Displays help information"
    , long          = "\2NICK\2 allows users to register a nickname and prevent others from using that nick. \2NICK\2 allows the owner of a nick to disconnect a user that is using their nick."
    , aboutSyntax   = "For more information on a topic, type"
    , args          = [Optional "topic", Optionals "subtopic"]
    , subTopics     = mkSubTopics
    }

mkSubTopics :: Arata Topics
mkSubTopics = getCommands "nickserv" >>= return . \case
    Nothing   -> []
    Just cmds -> mapMaybe f (M.elems cmds)
  where f (Alias _ _)      = Nothing
        f cmd@(Command {}) = Just (Topic (name cmd) (short cmd) (long cmd))
    --Just cmds -> map (\cmd -> Topic (name cmd) (short cmd) (long cmd)) (M.elems cmds)

handler :: CommandH
handler src dst [] = handler' src dst ["HELP"]
handler src dst xs = handler' src dst (map (map toUpper) xs)

handler' :: CommandH
handler' src dst (x:_) = getCommand "nickserv" x >>= \case
    Nothing  -> protoNotice dst src ("No help information available for \2" ++ x ++ "\2.")
    Just cmd -> do
        nick' <- getConfig "nickserv" "nick"
        mapM_ (protoNotice dst src) (long cmd $:$ 60)
        protoNotice dst src " "
        mapM_ (protoNotice dst src) ((aboutSyntax cmd ++ ":") $:$ 60)
        protoNotice dst src $ "    \2/msg " ++ nick' ++ ' ' : name cmd ++ case argsToString (args cmd) of
                Nothing -> ""
                Just s  -> ' ' : s
            ++ "\2"
        subTopics cmd >>= \case
            [] -> return ()
            xs -> do
                protoNotice dst src " "
                mapM_ (protoNotice dst src) ((aboutTopics cmd ++ ":") $:$ 60)
                forM_ xs $ \(Topic name' short' _) -> protoNotice dst src ('\2' : name' ++ '\2' : replicate (n + 3 - length name') ' ' ++ short')
              where n = maximum (map (length . topicName) xs)
        case subCommands cmd of
            [] -> return ()
            xs -> do
                protoNotice dst src " "
                mapM_ (protoNotice dst src) ((aboutCommands cmd ++ ":") $:$ 60)
                forM_ xs $ \subCmd -> protoNotice dst src ('\2' : name subCmd ++ '\2' : replicate (n + 3 - length (name subCmd)) ' ' ++ short subCmd)
              where n = maximum (map (length . name) xs)
        protoNotice dst src " "
        protoNotice dst src "   End of HELP"
handler' src dst [] = protoNotice dst src "No help information available."

argsToString :: [CommandArg] -> Maybe String
argsToString [] = Nothing
argsToString (x:xs) = Just (foldr (\arg s -> s ++ ' ' : argToString arg) (argToString x) xs)

argToString :: CommandArg -> String
argToString (Required s)  = '<' : s ++ ">"
argToString (Optional s)  = '[' : s ++ "]"
argToString (Optionals s) = '[' : s ++ " [...]]"
