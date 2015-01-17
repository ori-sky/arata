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

module Arata.Helper where

import Data.Char (toUpper)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.IxSet as Ix
import Data.Acid
import Data.ByteString.Char8 (pack, unpack)
import Data.ConfigFile.Monadic
import Control.Monad.State
import Network.Connection
import Arata.Types
import Arata.Config
import Arata.DB

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

setBurst :: Arata () -> Arata ()
setBurst f = modify (\env -> env { burst = f })

setAcidState :: AcidState DBState -> Arata ()
setAcidState as = modify (\env -> env { acidState = as })

getConfig :: Get_C a => SectionSpec -> OptionSpec -> Arata a
getConfig section option = getSection section >>= return . ($ option)

getSection :: Get_C a => SectionSpec -> Arata (OptionSpec -> a)
getSection section = do
    cp <- gets configParser
    return (getConfig' cp section)

getClient :: String -> Arata (Maybe Client)
getClient uid = gets clients >>= return . M.lookup uid

addClient :: Client -> Arata ()
addClient cli = modify (\env -> env { clients = M.insert (uid cli) cli (clients env) })

firstAvailableID :: Arata Int
firstAvailableID = queryDB QueryAccounts >>= return . f 1 . map accId . Ix.toAscList (Ix.Proxy :: Ix.Proxy Int)
  where
    f n (x:xs)
        | n == x = f (succ n) xs
        | otherwise = n
    f n _ = n

getCommands :: String -> Arata (Maybe Commands)
getCommands s = gets servs >>= return . M.lookup s

getCommand :: String -> String -> Arata (Maybe Command)
getCommand s c = getCommands s >>= return . \case
    Nothing   -> Nothing
    Just cmds -> M.lookup (map toUpper c) cmds

addCommand :: String -> Command -> Arata ()
addCommand s c = do
    servs' <- gets servs
    case M.lookup s servs' of
        Nothing   -> modify (\env -> env { servs = M.insert s (M.singleton (name c) c) servs' })
        Just cmds -> return ()
