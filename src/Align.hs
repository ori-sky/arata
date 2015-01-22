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

module Align
( wrap
, ($:$)
, (.:.)
) where

import Data.Char (isSpace)

reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
reverseBreak f xs = (reverse before, reverse after)
  where (after, before) = break f (reverse xs)

wrap :: Eq a => (a -> Bool) -> Int -> [a] -> [[a]]
wrap f n line
    | length line <= n = [line]
    | any f beforeN = before : wrap f n (after ++ afterN)
    | otherwise        = beforeN : wrap f n afterN
  where (beforeN, afterN) = splitAt n line
        (before, after)   = reverseBreak f beforeN

($:$) :: String -> Int -> [String]
line $:$ n = wrap isSpace n line

(.:.) :: [a] -> Int -> [[a]]
[] .:. _ = []
l .:. n
    | length l <= n = [l]
    | otherwise = xs : rest .:. n
  where (xs, rest) = splitAt n l
