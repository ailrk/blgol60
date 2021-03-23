{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Symbol
  ( Symbol(..)
  , toSymbol
  , lookupSymbol
  , insertSymbol
  )
where

import Data.Hashable qualified as Hashable
import Data.HashMap.Strict qualified as HM
import GHC.IO (unsafePerformIO)


newtype SymbolTable = SymbolTable {getSymbolTable :: HashMap Text Int}


-- | Memoization table for symbol lookup.
memoref :: IORef SymbolTable
memoref = unsafePerformIO $ newIORef emptySymbol
{-# NOINLINE memoref #-}


data Symbol = Symbol
  { text :: Text
  , hash :: Int
  }
  deriving (Show, Read, Eq, Ord)


toSymbol :: Text -> IO Symbol
toSymbol text = do
  symbolTable <- readIORef memoref
  case lookupSymbol text symbolTable of
    Just symbol -> pure symbol
    Nothing -> do
      let n = Hashable.hash text
      memoref `writeIORef` insertSymbol text n symbolTable
      pure (Symbol text n)


emptySymbol :: SymbolTable
emptySymbol = SymbolTable HM.empty


lookupSymbol :: Text -> SymbolTable -> Maybe Symbol
lookupSymbol text symbolTable =
  case HM.lookup text symbolTable.getSymbolTable of
    Just n -> Just (Symbol text n)
    Nothing -> Nothing


insertSymbol :: Text -> Int -> SymbolTable -> SymbolTable
insertSymbol text hash = SymbolTable . HM.insert text hash . (.getSymbolTable)
