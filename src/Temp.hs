{-# LANGUAGE OverloadedStrings #-}

module Temp
  ( Label
  , Temp
  , newTemp
  , newNamedLabel
  , newLabel
  )
where

-- temporary refers to temporary registers
-- label refers to memory address.

import Data.Text qualified as T
import GHC.IO (unsafePerformIO)
import Symbol qualified


type Label = Symbol.Symbol
data Temp = Temp Int (Maybe T.Text)



counterTemp :: IORef Int
counterTemp = unsafePerformIO $ newIORef 0
{-# NOINLINE counterTemp #-}


newTemp :: IO Temp
newTemp = do
  i <- readIORef counterTemp
  modifyIORef counterTemp (+ 1)
  return $ Temp i (Just "t")


counterLabel :: IORef Int
counterLabel = unsafePerformIO $ newIORef 0
{-# NOINLINE counterLabel #-}


-- unique memory address label
newNamedLabel :: T.Text -> IO Label
newNamedLabel name = do
  i <- readIORef counterLabel
  modifyIORef counterLabel (+ 1)
  Symbol.toSymbol (T.concat [name, show i])


newLabel :: IO Label
newLabel = newNamedLabel "l"
