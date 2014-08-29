{- Copyright 2014 David Farrell <shokku.ra@gmail.com>

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

module Arata.Types where

import Data.ConfigFile.Monadic
import Control.Monad.State
import Network.Connection (Connection)

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

data Client = Client
    { uid       :: Integer
    , nick      :: String
    , ts        :: Int
    , user      :: String
    , realName  :: String
    , userModes :: [Char]
    , vHost     :: String
    , host      :: String
    , ip        :: String
    , account   :: Maybe String
    }

data Env = Env
    { connection    :: Connection
    , configParser  :: ConfigParser
    }
type Arata = StateT Env IO

defaultEnv :: Connection -> Env
defaultEnv con = Env
    { connection = con
    , configParser  = defaultCP
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
            >>= set "chanserv"  "host"          "chanserv.services.int"
            >>= set "chanserv"  "name"          "Channel Services"
            >>= add_section "nickserv"
            >>= set "nickserv"  "nick"          "NickServ"
            >>= set "nickserv"  "user"          "NickServ"
            >>= set "nickserv"  "host"          "nickserv.services.int"
            >>= set "nickserv"  "name"          "Nickname Services"
