{- Copyright 2014-2015 David Farrell <shokku.ra@gmail.com>

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
{-# LANGUAGE DeriveDataTypeable #-}

module Arata.Types where

import Data.Typeable
import qualified Data.Map as M
import Data.IxSet
import Data.Acid
import Data.ConfigFile.Monadic
import Control.Monad.State
import Network.Connection (Connection)
import Dated

data Message = Message
    { tags      :: () -- TODO: support message tags
    , prefix    :: Maybe Prefix
    , command   :: String
    , params    :: [String]
    } deriving Show

data Prefix = NoPrefix | StringPrefix String | MaskPrefix Hostmask deriving Eq

instance Show Prefix where
    show NoPrefix = "*"
    show (StringPrefix p) = p
    show (MaskPrefix p) = show p

data Hostmask = Hostmask
    { maskNick  :: String
    , maskUser  :: String
    , maskHost  :: String
    } deriving Eq

instance Show Hostmask where show (Hostmask n u h) = n ++ '!' : u ++ '@' : h

type PrivmsgH = Client -> Client -> [String] -> Arata ()

data Client = Client
    { uid       :: String
    , nick      :: String
    , ts        :: Int
    , user      :: String
    , realName  :: String
    , userModes :: [Char]
    , vHost     :: String
    , host      :: String
    , ip        :: String
    , cert      :: Maybe String
    , account   :: Maybe String
    , privmsgH  :: Maybe PrivmsgH
    }

data Env = Env
    { connection    :: Connection
    , configParser  :: ConfigParser
    , burst         :: Arata ()
    , clients       :: M.Map String Client
    , nextUid       :: Int
    , acidState     :: AcidState DBState
    , servs         :: M.Map String Commands
    }
type Arata = StateT Env IO

defaultEnv :: Connection -> Arata () -> AcidState DBState -> Env
defaultEnv con burst' as = Env
    { connection    = con
    , configParser  = defaultCP
    , burst         = burst'
    , clients       = M.empty
    , nextUid       = 1
    , acidState     = as
    , servs         = M.empty
    }

defaultCP :: ConfigParser
defaultCP = case eitherCP of
    Left e   -> error (show e)
    Right cp -> cp
  where eitherCP = return emptyCP
            >>= add_section "info"
            >>= set "info"      "id"            "0AR"
            >>= set "info"      "name"          "services.int"
            >>= set "info"      "description"   "Arata IRC Services"
            >>= add_section "local"
            >>= set "local"     "password"      "password"
            >>= add_section "remote"
            >>= set "remote"    "host"          "127.0.0.1"
            >>= set "remote"    "port"          "6697"
            >>= set "remote"    "tls"           "enabled"
            >>= set "remote"    "password"      "password"
            >>= add_section "chanserv"
            >>= set "chanserv"  "nick"          "ChanServ"
            >>= set "chanserv"  "user"          "ChanServ"
            >>= set "chanserv"  "name"          "Channel Services"
            >>= add_section "nickserv"
            >>= set "nickserv"  "nick"          "NickServ"
            >>= set "nickserv"  "user"          "NickServ"
            >>= set "nickserv"  "name"          "Nickname Services"

data AuthMethod = PassAuth String
                | CertAuth String
                deriving (Eq, Ord, Show)

data Account = Account
    { accId     :: Int
    , accName   :: String
    , emails    :: [Dated String]
    , auths     :: [Dated AuthMethod]
    } deriving (Eq, Ord, Typeable, Show)

instance Indexable Account where
    empty = ixSet [ixFun ((: []) . accId), ixFun ((: []) . accName)]

data DBState = DBState
    { accounts :: IxSet Account
    } deriving Typeable

defaultDBState :: DBState
defaultDBState = DBState
    { accounts = empty
    }

data Serv = Serv
    { servNick      :: String
    , servUser      :: String
    , servRealName  :: String
    , servHandler   :: Maybe PrivmsgH
    }

-- plugin API

type CommandH = Client -> Client -> [String] -> Arata ()

data CommandArg = Required String | Optional String | Optionals String

data Command = Command
    { name          :: String
    , commandH      :: CommandH
    , short         :: String
    , long          :: String
    , aboutSyntax   :: String
    , aboutTopics   :: String
    , aboutCommands :: String
    , args          :: [CommandArg]
    , subTopics     :: Arata Topics
    , subCommands   :: [Command]
    }

type Commands = M.Map String Command

defaultCommand :: String -> CommandH -> Command
defaultCommand n h = Command
    { name          = n
    , commandH      = h
    , short         = "Short command description"
    , long          = "This is the long description of a command."
    , aboutSyntax   = "Syntax"
    , aboutTopics   = "The following topics are available"
    , aboutCommands = "The following sub-commands are available"
    , args          = []
    , subTopics     = return []
    , subCommands   = []
    }

data Topic = Topic
    { topicName     :: String
    , topicShort    :: String
    , topicLong     :: String
    }

type Topics = [Topic]

data PluginExport = ServExport String
                  | CommandExport String Command

type Exports = [PluginExport]
