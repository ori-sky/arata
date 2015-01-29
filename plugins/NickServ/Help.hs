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
import Align
import Arata.Types
import Arata.Helper

exports = [ CommandExport "nickserv" cmdHelp
          , CommandExport "nickserv" (Alias "H" "HELP")
          ]

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

handler src dst [] = handler' src dst ["HELP"]
handler src dst xs = handler' src dst (map (map toUpper) xs)

handler' :: CommandH
handler' src dst (x:_) = getCommand "nickserv" x >>= \case
    Nothing  -> return [NoticeAction dst src ("No help information available for \2" ++ x ++ "\2.")]
    Just cmd -> do
        nick' <- getConfig "nickserv" "nick"
        subTs <- subTopics cmd
        let part1 = long cmd $:$ 60
                 ++ " "
                  : (aboutSyntax cmd ++ ":") $:$ 60
                 ++ ("    \2/msg " ++ nick' ++ ' ' : name cmd ++ maybe "" (' ' :) (argsToString (args cmd)) ++ "\2")
                  : []
            n f xs = maximum (map (length . f) xs)
            part2 = if null subTs
                then []
                else " " : (aboutTopics cmd ++ ":") $:$ 60
                        ++ map (\(Topic name' short' _) -> ('\2' : name' ++ '\2' : replicate (n topicName subTs + 3 - length name') ' ' ++ short')) subTs
            part3 = case subCommands cmd of
                [] -> []
                xs -> " " : (aboutCommands cmd ++ ":") $:$ 60
                         ++ map (\subCmd -> ('\2' : name subCmd ++ '\2' : replicate (n name xs + 3 - length (name subCmd)) ' ' ++ short subCmd)) xs
            part4 = [" ", "  End of HELP"]
        return $ map (NoticeAction dst src) (part1 ++ part2 ++ part3 ++ part4)
handler' src dst _ = return [NoticeAction dst src "No help information available."]

argsToString :: [CommandArg] -> Maybe String
argsToString [] = Nothing
argsToString (x:xs) = Just (foldr (\arg s -> s ++ ' ' : argToString arg) (argToString x) xs)

argToString :: CommandArg -> String
argToString (Required s)  = '<' : s ++ ">"
argToString (Optional s)  = '[' : s ++ "]"
argToString (Optionals s) = '[' : s ++ " [...]]"
