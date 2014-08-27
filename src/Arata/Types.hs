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

data Message = Message
    { tags      :: () -- TODO: support message tags
    , prefix    :: Maybe Prefix
    , command   :: String
    , params    :: [String]
    } deriving Show

data Prefix = StringPrefix String | MaskPrefix Hostmask deriving Eq

instance Show Prefix where
    show (StringPrefix p) = p
    show (MaskPrefix p) = show p

data Hostmask = Hostmask
    { maskNick  :: String
    , maskUser  :: String
    , maskHost  :: String
    } deriving Eq

instance Show Hostmask where show (Hostmask n u h) = n ++ '!' : u ++ '@' : h