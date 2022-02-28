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
{-# LANGUAGE OverloadedStrings #-}

module Temp where

-- temporary refers to temporary registers
-- label refers to memory address.

import           Data.IORef
import qualified Data.Text  as T
import           GHC.IO     (unsafePerformIO)
import qualified Symbol

type Label = Symbol.Symbol
data Temp = Temp Int (Maybe T.Text)

instance Show Temp where
  show (Temp n Nothing)  = 't' : show n
  show (Temp n (Just t)) = show t ++ show n

-- unique temporary register
newTemp :: IO Temp
newTemp = do
  i <- readIORef counterTemp
  modifyIORef counterTemp (+1)
  return $ Temp i (Just "t")
  where
    counterTemp :: IORef Int
    counterTemp = unsafePerformIO $ newIORef 0

-- unique memory address label
newNamedLabel :: T.Text -> IO Label
newNamedLabel name = do
  i <- readIORef counterLabel
  modifyIORef counterLabel (+1)
  sym <- Symbol.toSymbol (T.concat [name, T.pack $ show i])
  return $ sym

newLabel = newNamedLabel "l"

counterLabel :: IORef Int
counterLabel = unsafePerformIO $ newIORef 0
