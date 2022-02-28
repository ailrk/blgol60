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

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Symbol where
import           Control.Monad.State
import qualified Data.HashMap.Lazy   as HM
import           Data.Hashable       (Hashable (hash))
import           Data.IORef
import qualified Data.Map            as M
import           Data.String         (IsString)
import qualified Data.Text           as T
import           GHC.IO              (unsafePerformIO)
import           Prelude             hiding (lookup)

-- symbol is for performance consideration. We don't want to performance
-- unecessary character comparisons, instead we hash each name and compare
-- the hash directly..


-- getting a fresh name. it's a global property.

type Symbol = (T.Text, Int)

class IsSymbol sym where
  type Str sym
  type Hash sym
  type Table sym a
  type Memo sym

  toSymbol :: Str sym -> IO sym
  empty :: Table sym a
  enter :: Table sym a -> sym -> a -> Table sym a
  lookup :: Table sym a -> sym -> Maybe a

instance IsSymbol Symbol where
  type Str Symbol = T.Text
  type Hash Symbol = Int
  type Table Symbol a = M.Map Int a
  type Memo Symbol  = HM.HashMap T.Text Int

  -- fast conversion to symbol, memoize the result
  toSymbol t = do
    memo <- readIORef memoref
    -- putStrLn $ show memo
    case memo HM.!? t of
      Just n  -> return $ (t, n)
      Nothing -> do
        let n = hash t
        let memo1 = (HM.insert t n memo)
        -- putStrLn $ show memo1
        writeIORef memoref memo1
        return $ (t, n)

  empty = M.empty
  enter m (_, h) a = M.insert h a m
  lookup m (_, h) = M.lookup h m

-- global mutable state, whatever.
memoref :: IORef (HM.HashMap T.Text Int)
memoref = unsafePerformIO (newIORef HM.empty)
