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
import Control.Monad.State (modify)
import Control.Monad.Reader (ask)
import Arata.Types
import Arata.Helper

queryAccountsById :: Int -> Query DBState (IxSet Account)
queryAccountsById id' = (@= id') . accounts <$> ask

addAccount :: Account -> Update DBState ()
addAccount acc = modify (\s -> s { accounts = insert acc (accounts s) })

$(deriveSafeCopy 0 'base ''Account)
$(deriveSafeCopy 0 'base ''DBState)
$(makeAcidic ''DBState ['queryAccountsById, 'addAccount])
