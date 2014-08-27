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

import Network
import System.IO
import Arata.Types
import Arata.Message

(>>:) :: Monad m => m a -> [a -> m b] -> m [b]
left >>: right = do
    b <- left
    mapM (\f -> f b) right
infixl 1 >>:

main :: IO ()
main = do
    handle <- connectTo "127.0.0.1" (PortNumber 6667)
    hSetBuffering handle NoBuffering
    hSetNewlineMode handle (NewlineMode CRLF CRLF)
    send handle "PASS password TS 6 :0AR"
    send handle "CAPAB :EX IE QS ENCAP"
    send handle "SERVER services.int 1 :Arata IRC Services"
    loop handle

loop :: Handle -> IO ()
loop handle = do
    hGetLine handle >>: [putStrLn . ("<- " ++), handleMessage handle . parseMessage]
    loop handle

send :: Handle -> String -> IO ()
send handle line = do
    putStrLn ("-> " ++ line)
    hPutStrLn handle line

handleMessage :: Handle -> Message -> IO ()
handleMessage handle (Message _ _ "PING" (server1:_)) = send handle ("PONG :" ++ server1)
handleMessage _ _ = return ()
