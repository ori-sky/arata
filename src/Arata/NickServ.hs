module Arata.NickServ where

import Data.Maybe
import Data.Char (toUpper)
import Data.IxSet as Ix
import Data.Time.Clock
import Control.Monad.State (liftIO)
import Align
import Dated
--import Arata.Types (Serv(..), servNick, servUser, servRealName, servHandler)
import Arata.Types
import Arata.Helper
import Arata.DB
import Arata.Protocol.Charybdis

serv :: Arata Serv
serv = do
    ns <- getSection "nickserv"
    return $ Serv
        { servNick          = ns "nick"
        , servUser          = ns "user"
        , servRealName      = ns "name"
        , servHandler       = Just handler
        }

handler :: PrivmsgH

handler src dst ["HELP"] = nsHelp src dst ["HELP"]
handler src dst ("HELP":x:xs) = nsHelp src dst (map toUpper x : xs)

handler src dst ["REGISTER"] = nsRegister src dst Nothing Nothing >>= mapM_ (protoNotice dst src) . snd
handler src dst ["REGISTER", pass] = nsRegister src dst (Just pass) Nothing >>= mapM_ (protoNotice dst src) . snd
handler src dst ("REGISTER":pass:email:_)
    | '@' `elem` email = nsRegister src dst (Just pass) (Just email) >>= mapM_ (protoNotice dst src) . snd
    | otherwise = protoNotice dst src ('\2' : email ++ "\2 is not a valid email address.")

handler src dst ("DROP":_) = nsDrop src dst >>= mapM_ (protoNotice dst src) . snd

handler src dst ["LOGIN"] = nsLogin src dst Nothing Nothing >>= mapM_ (protoNotice dst src) . snd
handler src dst ["LOGIN", pass] = nsLogin src dst Nothing (Just pass) >>= mapM_ (protoNotice dst src) . snd
handler src dst ("LOGIN":accName:pass:_) = nsLogin src dst (Just accName) (Just pass) >>= mapM_ (protoNotice dst src) . snd

handler src dst ("LOGOUT":_) = nsLogout src dst >>= mapM_ (protoNotice dst src) . snd

handler src dst ["ADD"] = do
    nick' <- getConfig "nickserv" "nick"
    protoNotice dst src "Not enough parameters for \2ADD\2."
    protoNotice dst src "Syntax: ADD <option> <parameters>"
    protoNotice dst src ("For more information, type \2/msg " ++ nick' ++ " HELP ADD")
handler src dst ("ADD":x:xs) = nsAdd src dst (map toUpper x : xs)

handler src dst _ = do
    nick' <- getConfig "nickserv" "nick"
    protoNotice dst src ("Invalid command. Use \2/msg " ++ nick' ++ " HELP\2 for a list of valid commands.")

-- stage 3 handlers

nsHelp :: PrivmsgH
nsHelp src dst ("HELP":_) = do
    nick' <- getConfig "nickserv" "nick"
    let msg = '\2' : nick' ++ "\2 allows users to register a nickname and prevents others from using that nick. \2" ++ nick' ++ "\2 allows the owner of a nick to disconnect a user that is using their nick."
    mapM_ (protoNotice dst src) (msg $:$ 60)
    protoNotice dst src " "
    protoNotice dst src "For more information about a command, type:"
    protoNotice dst src ("    \2/msg " ++ nick' ++ " HELP <command>")
    protoNotice dst src " "
    protoNotice dst src "The following commands are available:"
    protoNotice dst src "\2ADD\2        Adds a property to your account"
    protoNotice dst src "\2CONFIRM\2    Confirms a previous command"
    protoNotice dst src "\2DEL\2        Removes a property from your account"
    protoNotice dst src "\2DROP\2       Drops your account"
    protoNotice dst src "\2GROUP\2      Adds a nick to your account"
    protoNotice dst src "\2HELP\2       Displays help information"
    protoNotice dst src "\2INFO\2       Displays account information"
    protoNotice dst src "\2LOGIN\2      Logs into an account"
    protoNotice dst src "\2LOGOUT\2     Logs out of your account"
    protoNotice dst src "\2NICK\2       Recovers a nick and changes your nick to it"
    protoNotice dst src "\2RECOVER\2    Recovers a nick grouped to your account"
    protoNotice dst src "\2REGISTER\2   Registers a new account"
    protoNotice dst src "\2SHOW\2       Shows account properties"
    protoNotice dst src "\2UNGROUP\2    Removes a nick from your account"
    protoNotice dst src " "
nsHelp src dst (x:_) = protoNotice dst src ("No help information available for \2" ++ x ++ "\2.")
nsHelp src dst [] = protoNotice dst src "No help information available."

nsAdd :: PrivmsgH
nsAdd src dst ["PASSWORD"] = do
    protoNotice dst src "Not enough parameters for \2ADD PASSWORD\2."
    protoNotice dst src "Syntax: ADD PASSWORD <password>"
nsAdd src dst ("PASSWORD":pass:_) = do
    (succeeded, notices) <- nsAddAuth src dst (PassAuth pass)
    if succeeded
        then protoNotice dst src ("The password \2" ++ pass ++ "\2 has been added to your account.")
        else mapM_ (protoNotice dst src) notices

nsAdd src dst ("MYCERT":_) = case cert src of
    Nothing -> protoNotice dst src "You are not connected with an SSL certificate."
    Just cert' -> do
        (succeeded, notices) <- nsAddAuth src dst (CertAuth cert')
        if succeeded
            then protoNotice dst src ("The SSL certificate \2" ++ cert' ++ "\2 has been added to your account.")
            else mapM_ (protoNotice dst src) notices

nsAdd src dst _ = do
    nick' <- getConfig "nickserv" "nick"
    protoNotice dst src ("Invalid option for \2ADD\2. Use \2/msg " ++ nick' ++ " HELP ADD\2 for a list of valid options.")

-- nickserv functions

nsRegister :: Client -> Client -> Maybe String -> Maybe String -> Arata (Bool, [String])
nsRegister src _ pass email = do
    accs <- queryDB $ QueryAccountsByNick (nick src)
    if Ix.null accs
        then do
            accId' <- firstAvailableID
            updateDB $ AddAccount (Account accId' (nick src) [] [])
            protoAuthClient src (Just (nick src))
            return (True, [msg])
        else return (False, ['\2' : nick src ++ "\2 is already registered."])
  where msgEmail = " to \2" ++ (fromJust email) ++ "\2"
        msgPass = " with the password \2" ++ (fromJust pass) ++ "\2"
        msg = '\2' : nick src ++ "\2 is now registered"
            ++ (if isJust email then msgEmail else "")
            ++ (if isJust pass then msgPass else "")
            ++ "."

nsDrop :: Client -> Client -> Arata (Bool, [String])
nsDrop _ _ = return (False, ["This command is not implemented."])

nsLogin :: Client -> Client -> Maybe String -> Maybe String -> Arata (Bool, [String])
nsLogin src _ Nothing Nothing
    | isJust (account src) = return (False, ["You are already logged in as \2" ++ fromJust (account src) ++ "\2."])
    | isNothing (cert src) = return (False, ["Failed to login to \2" ++ nick src ++ "\2."])
    | otherwise = do
        accs <- queryDB $ QueryAccountsByNick (nick src)
        if Ix.null accs
            then return (False, ['\2' : nick src ++ "\2 is not registered."])
            else if f (auths (fromJust (getOne accs)))
                then do
                    protoAuthClient src (Just (nick src))
                    return (True, ["You are now logged in as \2" ++ nick src ++ "\2."])
                else return (False, ["Failed to login to \2" ++ nick src ++ "\2."])
  where f (CertAuth cert' :@ _ : xs)
            | fromJust (cert src) == cert' = True
            | otherwise = f xs
        f [] = False
        f (_:xs) = f xs
nsLogin src dst Nothing pass = nsLogin src dst (Just (nick src)) pass
nsLogin src _ (Just accName) (Just pass)
    | isJust (account src) = return (False, ["You are already logged in as \2" ++ fromJust (account src) ++ "\2."])
    | otherwise = do
        accs <- queryDB $ QueryAccountsByNick (nick src)
        if Ix.null accs
            then return (False, ['\2' : accName ++ "\2 is not registered."])
            else if f (auths (fromJust (getOne accs)))
                then do
                    protoAuthClient src (Just accName)
                    return (True, ["You are now logged in as \2" ++ accName ++ "\2."])
                else return (False, ["Failed to login to \2" ++ accName ++ "\2."])
  where f (PassAuth p :@ _ : xs)
            | pass == p = True
            | otherwise = f xs
        f [] = False
        f (_:xs) = f xs
nsLogin _ _ _ _ = fail "Something went wrong"

nsLogout :: Client -> Client -> Arata (Bool, [String])
nsLogout src _ = case account src of
    Nothing -> return (False, ["You are not logged in."])
    Just _  -> do
        protoAuthClient src Nothing
        return (True, ["You have been logged out."])

nsAddAuth :: Client -> Client -> AuthMethod -> Arata (Bool, [String])
nsAddAuth src _ auth
    | account src == Nothing = return (False, ["You are not logged in."])
    | otherwise = do
        accs <- queryDB $ QueryAccountsByName (fromJust (account src))
        case getOne accs of
            Nothing  -> fail "[FATAL] client account not found in database"
            Just acc -> do
                t <- liftIO getCurrentTime
                updateDB $ UpdateAccount (acc { auths = (auth :@ t) : auths acc })
                return (True, ["The authentication method has been added to your account."])
