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

module Arata.Helper where

import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.ConfigFile.Monadic
import Control.Monad.State
import Network.Connection
import Arata.Types
import Arata.Config

send :: String -> Arata ()
send line = do
    liftIO $ putStrLn ("<- " ++ line)
    con <- gets connection
    liftIO $ connectionPut con (pack (line ++ "\r\n"))

recv :: Arata (Maybe String)
recv = do
    con <- gets connection
    liftIO (connectionGetChunk' con f)
  where f bs
            | BS.isInfixOf (pack "\r\n") bs = (Just (unpack s), BS.drop 2 ss)
            | otherwise = (Nothing, bs)
          where (s, ss) = BS.breakSubstring (pack "\r\n") bs

setEnvConfigParser :: ConfigParser -> Arata ()
setEnvConfigParser cp = modify (\env -> env { configParser = cp })

getConfig :: Get_C a => SectionSpec -> OptionSpec -> Arata a
getConfig section option = do
    cp <- gets configParser
    return (getConfig' cp section option)