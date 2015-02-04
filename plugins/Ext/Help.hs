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

{-# LANGUAGE DeriveDataTypeable #-}

module Ext.Help
( module Data.Dynamic
, ExtHelp(..)
, defaultExtHelp
, Topic(..)
, Topics
) where

import Data.Dynamic (Typeable(..), toDyn, fromDyn, fromDynamic)
import Arata.Types (Arata, Command)

data ExtHelp = ExtHelp
    { short         :: String
    , long          :: String
    , aboutSyntax   :: String
    , aboutTopics   :: String
    , aboutCommands :: String
    , subTopics     :: Arata Topics
    } deriving Typeable

defaultExtHelp = ExtHelp
    { short         = "Short command description"
    , long          = "This is the long description of a command."
    , aboutSyntax   = "Syntax"
    , aboutTopics   = "The following topics are available"
    , aboutCommands = "The following sub-commands are available"
    , subTopics     = return []
    }

data Topic = Topic
    { topicName     :: String
    , topicShort    :: String
    , topicLong     :: String
    }

type Topics = [Topic]
