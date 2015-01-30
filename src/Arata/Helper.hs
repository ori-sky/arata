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

setAcidState :: AcidState DBState -> Arata ()
setAcidState as = modify (\env -> env { acidState = as })

setFromProtocol :: FromProtocol -> Arata ()
setFromProtocol from = modify (\env -> env { fromProtocol = from })

setToProtocol :: ToProtocol -> Arata ()
setToProtocol to = modify (\env -> env { toProtocol = to })

setMakeUid :: MakeUid -> Arata ()
setMakeUid mkUid = modify (\env -> env { makeUid = mkUid })

getConfig :: Get_C a => SectionSpec -> OptionSpec -> Arata a
getConfig section option = getSection section >>= return . ($ option)

getSection :: Get_C a => SectionSpec -> Arata (OptionSpec -> a)
getSection section = do
    cp <- gets configParser
    return (getConfig' cp section)

getClient :: String -> Arata (Maybe Client)
getClient uid' = gets clients >>= return . M.lookup uid'

getClientByNick :: String -> Arata (Maybe Client)
getClientByNick nick' = gets clients >>= return . M.filter (pred' . nick) >>= \clis -> if M.null clis
    then liftIO (print nick') >> return Nothing
    else return $ Just (snd (M.elemAt 0 clis))
  where pred' = (== map toUpper nick') . map toUpper

addClient :: Client -> Arata ()
addClient cli = modify (\env -> env { clients = M.insert (uid cli) cli (clients env) })

firstAvailableID :: Arata Integer
firstAvailableID = queryDB QueryAccounts >>= return . f 1 . map accId . Ix.toAscList (Ix.Proxy :: Ix.Proxy Integer)
  where
    f n (x:xs)
        | n == x = f (succ n) xs
        | otherwise = n
    f n _ = n

getServ :: String -> Arata (Maybe String)
getServ uid' = gets servs >>= return . M.lookup uid'

addServ :: String -> String -> Arata ()
addServ uid' name' = modify (\env -> env { servs = M.insert uid' name' (servs env) })

getCommands :: String -> Arata (Maybe Commands)
getCommands s = gets servCommands >>= return . M.lookup s

getCommand :: String -> String -> Arata (Maybe Command)
getCommand = getCommand' 0

getCommand' :: Int -> String -> String -> Arata (Maybe Command)
getCommand' 5 _ _ = return Nothing
getCommand' n servName c = getCommands servName >>= \case
    Nothing   -> return Nothing
    Just cmds -> case M.lookup (map toUpper c) cmds of
        Nothing             -> return Nothing
        Just (Alias _ real) -> getCommand' (n + 1) servName real
        Just cmd            -> return (Just cmd)

addCommand :: String -> Command -> Arata ()
addCommand servName cmd@(Alias alias _) = addCommand' servName alias cmd
addCommand servName cmd@(Command {})    = addCommand' servName (name cmd) cmd

addCommand' :: String -> String -> Command -> Arata ()
addCommand' servName cmdName cmd = do
    servs' <- gets servCommands
    case M.lookup servName servs' of
        Nothing   -> modify (\env -> env { servCommands = M.insert servName (M.singleton cmdName cmd) servs' })
        Just cmds -> modify (\env -> env { servCommands = M.insert servName (M.insert cmdName cmd cmds) servs' })
