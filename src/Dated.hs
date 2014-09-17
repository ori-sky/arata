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

module Dated where

import Data.Time.Clock (UTCTime(..))
import Data.SafeCopy

data Dated a = a :@ UTCTime deriving (Eq, Ord)

instance SafeCopy a => SafeCopy (Dated a) where
    putCopy (x :@ t) = contain $ safePut (x, t)
    getCopy = contain $ do
        (x, t) <- safeGet
        return (x :@ t)
