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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Arata.Types where

import IRC.RFC1459 (toLower)
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
    }

instance Show Message where
    show (Message () maybePfx cmd args') = maybe "" (\pfx -> ':' : show pfx ++ " ") maybePfx ++ cmd ++ f args'
      where f [] = ""
            f [x] = ' ' : ':' : x
            f (x:xs) = ' ' : x ++ f xs

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

data Client = Client
    { uid       :: String
    , nick      :: String
    , ts        :: Integer
    , user      :: String
    , realName  :: String
    , userModes :: [Char]
    , vHost     :: String
    , host      :: String
    , ip        :: String
    , cert      :: Maybe String
    , account   :: Maybe String
    }

data Env = Env
    { connection    :: Connection
    , configParser  :: ConfigParser
    , acidState     :: AcidState DBState
    , pluginExports :: [PluginExport]
    , fromProtocol  :: FromProtocol
    , toProtocol    :: ToProtocol
    , makeUid       :: MakeUid
    , nextUid       :: Integer
    , clients       :: M.Map String Client
    , servs         :: M.Map String String
    , servCommands  :: M.Map String Commands
    }

type Arata = StateT Env IO

defaultEnv :: Connection -> AcidState DBState -> Env
defaultEnv con as = Env
    { connection    = con
    , configParser  = defaultCP
    , acidState     = as
    , pluginExports = []
    , fromProtocol  = const (return [])
    , toProtocol    = const (return [])
    , makeUid       = return . show
    , nextUid       = 1
    , clients       = M.empty
    , servs         = M.empty
    , servCommands  = M.empty
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
            >>= set "info"      "plugins"       "Protocol.Charybdis\
                                               \ NickServ\
                                               \ NickServ.Add\
                                               \ NickServ.Confirm\
                                               \ NickServ.Del\
                                               \ NickServ.Drop\
                                               \ NickServ.Group\
                                               \ NickServ.Help\
                                               \ NickServ.Info\
                                               \ NickServ.Login\
                                               \ NickServ.Logout\
                                               \ NickServ.Nick\
                                               \ NickServ.Recover\
                                               \ NickServ.Register\
                                               \ NickServ.Show\
                                               \ NickServ.Ungroup\
                                               \ ChanServ"
            >>= add_section "local"
            >>= set "local"     "password"      "password"
            >>= add_section "remote"
            >>= set "remote"    "host"          "127.0.0.1"
            >>= set "remote"    "port"          "6697"
            >>= set "remote"    "tls"           "enabled"
            >>= set "remote"    "password"      "password"
            >>= add_section "nickserv"
            >>= set "nickserv"  "nick"          "NickServ"
            >>= set "nickserv"  "user"          "NickServ"
            >>= set "nickserv"  "name"          "Nickname Services"
            >>= add_section "chanserv"
            >>= set "chanserv"  "nick"          "ChanServ"
            >>= set "chanserv"  "user"          "ChanServ"
            >>= set "chanserv"  "name"          "Channel Services"

data AuthMethod = PassAuth String
                | CertAuth String
                deriving (Eq, Ord, Show)

data Account = Account
    { accId     :: Integer
    , accName   :: String
    , emails    :: [Dated String]
    , auths     :: [Dated AuthMethod]
    } deriving (Eq, Ord, Typeable, Show)

instance Indexable Account where
    empty = ixSet [ixFun ((: []) . accId), ixFun ((: []) . map toLower . accName)]

data DBState = DBState
    { accounts :: IxSet Account
    } deriving Typeable

defaultDBState :: DBState
defaultDBState = DBState
    { accounts = empty
    }

-- plugin API

data PluginExport = ProtocolExport FromProtocol ToProtocol MakeUid
                  | ServExport String
                  | CommandExport String Command
                    deriving Typeable

type Exports = [PluginExport]

type FromProtocol = Message -> Arata [Event]
type ToProtocol = Action -> Arata [Message]
type MakeUid = Integer -> Arata String

data Event = PassEvent String
           | RegistrationEvent String
           | PingEvent String
           | IntroductionEvent String String String String String String String [Char] (Maybe String) (Maybe Integer)
           | CertEvent String String
           | NickEvent String String (Maybe Integer)
           | PrivmsgEvent String String String
             deriving Show

data Action = RegistrationAction String String (Maybe String)
            | QuitAction String String
            | PongAction String
            | IntroductionAction String String String String String (Maybe String) Integer
            | AuthAction String (Maybe String)
            | PrivmsgAction String String String
            | NoticeAction String String String
              deriving Show

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
    }        | Alias String String

data CommandArg = Required String | Optional String | Optionals String
type CommandH = String -> String -> [String] -> Arata [Action]
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
