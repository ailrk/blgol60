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
{-# LANGUAGE LambdaCase   #-}

-- a read black tree based map.
module Map where

import           Prelude hiding (fold, lookup, map)

type Size = Int

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
    go Tip = Nothing
    go (Bin _ k1 v1 l r) =
      case compare k k1 of
        LT -> go l
        GT -> go r
        EQ -> Just v1

member :: Ord k => k -> Map k a -> Bool
member k m = case lookup k m of
             Nothing -> False
             Just _  -> True

find :: Ord k => k -> Map k a -> a
find k m = case lookup k m of
             Just v  -> v
             Nothing -> error "not found"

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault def k m = case lookup k m of
                            Just v  -> v
                            Nothing -> def

empty :: Map k a
empty = Tip

singleton :: k -> a -> Map k a
singleton k v = Bin 1 k v Tip Tip

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k v l r  = Bin (size l + size r + 1) k v l r

delta, ratio :: Int
delta = 4
ratio = 2

size :: Map k a -> Size
size Tip              = 0
size (Bin sz _ _ _ _) = sz

balance, rotateL, rotateR, singleL, singleR, doubleL, doubleR :: k -> v -> Map k v -> Map k v -> Map k v

balance k v l r
  | (size l) + (size r) <= 1 = Bin (size l+ size r + 1)  k v l r
  | (size r) >= delta * (size l) = rotateL k v l r
  | (size l) >= delta * (size r) = rotateR k v l r
  | otherwise = Bin (size l + size r + 1) k v l r

rotateL k x l r@(Bin _ _ _ ly ry)
  | size ly < ratio * size ry = singleL k x l r
  | otherwise = doubleL k x l r
rotateR k x l@(Bin _ _ _ ly ry) r
  | size ry < ratio * size ly  = singleR k x l r
  | otherwise = doubleR k x l r

singleL k1 v1 t1 (Bin _ k2 v2 t2 t3) = bin k2 v2 (bin k1 v2 t1 t2) t3
singleL _ _ _ Tip                    = error "singleL Tip"
singleR k1 v1 (Bin _ k2 v2 t1 t2) t3 = bin k2 v2 t1 (bin k1 v2 t2 t3)
singleR _ _ _ Tip                    = error "singleR Tip"
doubleL k1 v1 t1 (Bin _ k2 v2 (Bin _ k3 v3 t2 t3) t4) = bin k3 v3 (bin k1 v1 t1 t2) (bin k2 v2 t3 t4)
doubleL _ _ _ Tip = error "doubleL Tip"
doubleR k1 v1 (Bin _ k2 v2 t1 (Bin _ k3 v3 t2 t3)) t4 = bin k3 v3 (bin k2 v2 t1 t2) (bin k1 v1 t3 t4)
doubleR _ _ _ Tip = error "doubleR Tip"

balanced :: Map k a -> Bool
balanced =
  \case
    Tip -> True
    Bin _ _ _ l r -> (size l + size r <= 1 || (size l <= delta * size r && size r <= delta * size l))
                  && balanced l
                  && balanced r

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k v = k `seq` go
  where
    go Tip = singleton k v
    go (Bin sz k1 v1 l r) =
      case compare k k1 of
        LT -> balance k1 v1 (go l) r
        GT -> balance k1 v1 l (go r)
        EQ -> Bin sz k1 v1 l r

deleteMin :: Map k a -> ((k, a), Map k a)
deleteMin t =
  case t of
    Bin _ k v Tip r -> ((k, v), r)
    Bin _ k v l r -> let (m, l1) = deleteMin l in (m, balance k v l1 r)
    Tip -> (error "Can't find min for Tip", Tip)

deleteMax :: Map k a -> ((k, a), Map k a)
deleteMax t =
  case t of
    Bin _ k v l Tip -> ((k, v), l)
    Bin _ k v l r -> let (m, r1) = deleteMax r in (m, balance k v l r1)
    Tip -> (error "Can't find max for Tip", Tip)


-- assume l and r trees are balanced.
glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let ((k, v), l1) = deleteMax l in balance k v l1 r
  | otherwise = let ((k, v), r1) = deleteMin r in balance k v l r1

delete :: Ord k => k -> Map k a -> Map k a
delete k = k `seq` go
  where
    go Tip = Tip
    go (Bin _ k1 v1 l r) =
      case compare k k1 of
        LT -> balance k1 v1 (go l) r
        GT -> balance k1 v1 l (go r)
        EQ -> glue l r

merge :: Map k a -> Map k a -> Map k a
merge Tip r = r
merge l Tip = l
merge l@(Bin szL kl vl ll rl) r@(Bin szR kr vr lr rr)
  | delta * szL <= szR = balance kr vr (merge l lr) rr
  | delta * szR <= szL = balance kl vl ll (merge rl r)
  | otherwise = glue l r

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
