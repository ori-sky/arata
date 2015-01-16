module Arata.ChanServ where

import Control.Monad.State (liftIO)
import Arata.Types
import Arata.Helper

serv :: Arata Serv
serv = do
    cs <- getSection "chanserv"
    return $ Serv
        { servNick      = cs "nick"
        , servUser      = cs "user"
        , servRealName  = cs "name"
        , servHandler   = Just handler
        }

handler :: PrivmsgH
handler _ _ (x:_) = liftIO (putStrLn "hello")
handler _ _ [] = return ()
