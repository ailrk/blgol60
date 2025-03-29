{-# LANGUAGE OverloadedStrings #-}

module Temp
  ( Label
  , Temp(..)
  , newNamedLabel
  , newLabel
  , newNamedTemp
  , newTemp
  )
where

-- temporary refers to temporary registers
-- label refers to memory address.

import Data.Text qualified as T
import Symbol qualified
import UnliftIO (MonadUnliftIO)
import Symbol (HasSymbolTable)


-- | Represents a virtual memory location
type Label = Symbol.Symbol

-- | Represent a virtual registor that holds a temporary value.
data Temp = Temp Int (Maybe T.Text)


class HasTempCounter ctx where
  getTempCounter :: ctx -> IORef Int


class HasLabelCounter ctx where
  getLabelCounter :: ctx -> IORef Int


-- unique memory address label
newNamedLabel :: (HasLabelCounter ctx, HasSymbolTable ctx, MonadReader ctx m, MonadUnliftIO m) => T.Text -> m Label
newNamedLabel name = do
  ref <- getLabelCounter <$> ask
  modifyIORef ref (+ 1)
  i <- readIORef ref
  Symbol.toSymbol (T.concat [name, show i])


newLabel :: (HasLabelCounter ctx, HasSymbolTable ctx, MonadReader ctx m, MonadUnliftIO m) => m Label
newLabel = newNamedLabel "l"


-- unique memory address label
newNamedTemp :: (HasTempCounter ctx, HasSymbolTable ctx, MonadReader ctx m, MonadUnliftIO m) => T.Text -> m Label
newNamedTemp name = do
  ref <- getTempCounter <$> ask
  modifyIORef ref (+ 1)
  i <- readIORef ref
  Symbol.toSymbol (T.concat [name, show i])


newTemp :: (HasTempCounter ctx, HasSymbolTable ctx, MonadReader ctx m, MonadUnliftIO m) => m Label
newTemp = newNamedTemp "t"
