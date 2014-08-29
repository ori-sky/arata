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

{-# LANGUAGE LambdaCase #-}

module Arata.Config where

import Data.ConfigFile.Monadic
import System.IO (hPutStrLn, stderr)
import Arata.Types

loadConfig' :: String -> IO ConfigParser
loadConfig' path = do
    (readfile path defaultCP :: IO (Either CPError ConfigParser)) >>= \case
        Left e   -> hPutStrLn stderr (show e) >> return defaultCP
        Right cp -> return cp

getConfig' :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> a
getConfig' cp section option = forceEither (get cp section option)

forceEither :: Show e => Either e a -> a
forceEither (Left x) = error (show x)
forceEither (Right x) = x
