{-# LANGUAGE OverloadedStrings #-}

module Temp
  ( Label
  , Temp
  , newNamedLabel
  , newLabel
  )
where

-- temporary refers to temporary registers
-- label refers to memory address.

import Data.Text qualified as T
import Symbol qualified
import UnliftIO (MonadUnliftIO)
import Symbol (HasSymbolTable)


type Label = Symbol.Symbol
data Temp = Temp Int (Maybe T.Text)


class HasTempCounter ctx where
  getTempCounter :: ctx -> IORef Int


-- unique memory address label
newNamedLabel :: (HasTempCounter ctx, HasSymbolTable ctx, MonadReader ctx m, MonadUnliftIO m) => T.Text -> m Label
newNamedLabel name = do
  ref <- getTempCounter <$> ask
  modifyIORef ref (+ 1)
  i <- readIORef ref
  Symbol.toSymbol (T.concat [name, show i])


newLabel :: (HasTempCounter ctx, HasSymbolTable ctx, MonadReader ctx m, MonadUnliftIO m) => m Label
newLabel = newNamedLabel "l"
