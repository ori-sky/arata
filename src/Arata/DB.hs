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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Arata.DB where

import Data.SafeCopy
import Data.IxSet
import Data.Acid
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (ask)
import Dated
import Arata.Types

queryAccountsById :: Int -> Query DBState (IxSet Account)
queryAccountsById x = (@= x) . accounts <$> ask

queryAccountsByNick :: String -> Query DBState (IxSet Account)
queryAccountsByNick x = (@= x) . accounts <$> ask

queryAccountsByName :: String -> Query DBState (IxSet Account)
queryAccountsByName x = (@= x) . accounts <$> ask

addAccount :: Account -> Update DBState ()
addAccount acc = modify (\s -> s { accounts = insert acc (accounts s) })

updateAccount :: Account -> Update DBState ()
updateAccount acc = modify (\s -> s { accounts = updateIx (accId acc) acc (accounts s) })

queryDB q = do
    as <- gets acidState
    liftIO (query as q)

updateDB u = do
    as <- gets acidState
    liftIO (update as u)

$(deriveSafeCopy 0 'base ''Dated)
$(deriveSafeCopy 0 'base ''AuthMethod)
$(deriveSafeCopy 0 'base ''Account)
$(deriveSafeCopy 0 'base ''DBState)
$(makeAcidic ''DBState ['queryAccountsById, 'queryAccountsByNick, 'queryAccountsByName, 'addAccount, 'updateAccount])