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
import Control.Monad (unless)
import Control.Monad.State (modify, execStateT, lift)
import Align
import Arata.Types hiding (short, long)
import Arata.Helper
import Ext.Help

exports = [CommandExport "nickserv" cmdHelp , CommandExport "nickserv" (Alias "H" "HELP")]

cmdHelp = (defaultCommand "HELP" handler)
    { args       = [Optional "topic", Optionals "subtopic"]
    , extensions = M.singleton (typeOf extHelp) (toDyn extHelp)
    }

extHelp = defaultExtHelp
    { short       = "Displays help information"
    , long        = "\2NICK\2 allows users to register a nickname and prevent others from using that nick. \2NICK\2 allows the owner of a nick to disconnect a user that is using their nick."
    , aboutSyntax = "For more information on a topic, type"
    , subTopics   = mkSubTopics
    }

mkSubTopics :: Arata Topics
mkSubTopics = getCommands "nickserv" >>= return . \case
    Nothing   -> []
    Just cmds -> mapMaybe f (M.elems cmds)
  where f (Alias _ _)      = Nothing
        f cmd@(Command {}) = do -- use Maybe as a Monad
            dynExt <- M.lookup (typeOf defaultExtHelp) (extensions cmd)
            ext <- fromDynamic dynExt
            return (Topic (name cmd) (short ext) (long ext))

handler src dst [] = handler' src dst ["HELP"]
handler src dst xs = handler' src dst (map (map toUpper) xs)

handler' :: CommandH
handler' src dst (x:_) = getCommand "nickserv" x >>= \case
    Nothing  -> return [NoticeAction dst src ("No help information available for \2" ++ x ++ "\2.")]
    Just cmd -> case M.lookup (typeOf defaultExtHelp) (extensions cmd) >>= fromDynamic of
        Nothing  -> return [NoticeAction dst src ("No help information available for \2" ++ x ++ "\2.")]
        Just ext -> do
            nick' <- getConfig "nickserv" "nick"
            subTs <- subTopics ext
            lines <- flip execStateT [] $ let app x = modify (++ x)
                                              n f xs = maximum (map (length . f) xs) in do
                app (long ext $:$ 60)
                app [" "]
                app ((aboutSyntax ext ++ ":") $:$ 60)
                app ["    \2/msg " ++ nick' ++ ' ' : name cmd ++ maybe "" (' ' :) (argsToString (args cmd)) ++ "\2"]
                unless (null subTs) $ do
                    app [" "]
                    app ((aboutTopics ext ++ ":") $:$ 60)
                    app $ map (\(Topic name' short' _) -> '\2' : name' ++ '\2' : replicate (n topicName subTs + 3 - length name') ' ' ++ short') subTs
                app [" ", "  End of HELP"]
            return $ map (NoticeAction dst src) lines
handler' src dst _ = return [NoticeAction dst src "No help information available."]

argsToString :: [CommandArg] -> Maybe String
argsToString [] = Nothing
argsToString (x:xs) = Just (foldr (\arg s -> s ++ ' ' : argToString arg) (argToString x) xs)

argToString :: CommandArg -> String
argToString (Required s)  = '<' : s ++ ">"
argToString (Optional s)  = '[' : s ++ "]"
argToString (Optionals s) = '[' : s ++ " [...]]"
