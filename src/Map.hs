-- doge-algol60
-- Copyright © 2021 ailrk
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the "Software"),
-- to deal in the Software without restriction, including without limitation
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following conditions:
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
-- OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
-- OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
{-# LANGUAGE BangPatterns #-}

-- a read black tree based map.
module Map where

import           Prelude hiding (fold, lookup, map)

type Size = Int

data Color = R | B deriving (Show, Eq)

data Map k a
  = Tip
  | Bin {-# UNPACK #-} !Size k a !(Map k a) !(Map k a)

instance Ord k => Semigroup (Map k v) where
  (<>) = undefined

instance Ord k => Monoid (Map k v) where
  mempty = undefined

null :: Map k a -> Bool
null Tip    = True
null Bin {} = False

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k = k `seq` go
  where
    go = undefined

member :: Ord k => k -> Map k a -> Bool
member = undefined

find :: Ord k => k -> Map k a -> a
find = undefined

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault = undefined

empty :: Map k a
empty = undefined

singleton :: k -> a -> Map k a
singleton = undefined

insert :: Ord k => k -> a -> Map k a -> Map k a
insert = undefined

delete :: Ord k => k -> Map k a -> Map k a
delete = undefined

update :: Ord k => (a -> Map k a) -> k -> Map k a -> Map k a
update = undefined

unions :: Ord k => [Map k a] -> Map k a
unions = undefined

union :: Ord k => Map k a -> Map k a -> Map k a
union = undefined

difference :: Ord k => Map k a -> Map k b -> Map k a
difference = undefined

intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection = undefined

isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOf = undefined

map :: (a -> b) -> Map k a -> Map k b
map = undefined

instance Functor (Map k) where
  fmap = map

mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccum = undefined

fold :: (a -> b -> b) -> b -> Map k a -> b
fold = undefined

elems :: Map k a -> [a]
elems = undefined

keys :: Map k a -> [k]
keys = undefined

assocs :: Map k a -> [(k, a)]
assocs = undefined

fromList :: Ord k => [(k, a)] -> Map k a
fromList = undefined

toList :: Map k a -> [(k, a)]
toList = undefined

