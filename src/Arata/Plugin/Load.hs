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

module Arata.Plugin.Load where

import Data.Dynamic (fromDyn)
import qualified GHC
import GHC.Paths (libdir)
import DynFlags
import Arata.Types (Exports)

load :: String -> IO Exports
load name = GHC.runGhc (Just libdir) $ do
    dflags <- GHC.getSessionDynFlags
    GHC.setSessionDynFlags dflags { importPaths = ["plugins", "src"] }
    target <- GHC.guessTarget name Nothing
    GHC.setTargets [target]
    GHC.load GHC.LoadAllTargets >>= \case
        GHC.Failed    -> fail "failed to load target module"
        GHC.Succeeded -> do
            GHC.setContext [GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName name)) { GHC.ideclQualified = True }]
            result <- GHC.dynCompileExpr (name ++ ".exports")
            return (fromDyn result [])
