{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Symbol where

import Data.HashMap.Lazy qualified as HM
import Data.Hashable qualified as Hashable
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import GHC.IO (unsafePerformIO)
import Prelude hiding (lookup)


newtype SymbolTable = SymbolTable {getSymbolTable :: HM.HashMap Text Int}


-- | Memoization table for symbol lookup.
memoref :: IORef SymbolTable
memoref = unsafePerformIO $ newIORef empty
{-# NOINLINE memoref #-}


data Symbol = Symbol
  { text :: Text
  , hash :: Int
  }
  deriving (Show, Read, Eq, Ord)


toSymbol :: Text -> IO Symbol
toSymbol text = do
  symbolTable <- readIORef memoref
  case lookup text symbolTable of
    Just symbol -> pure symbol
    Nothing -> do
      let n = Hashable.hash text
      memoref `writeIORef` insert text n symbolTable
      pure (Symbol text n)


empty :: SymbolTable
empty = SymbolTable HM.empty


lookup :: Text -> SymbolTable -> Maybe Symbol
lookup text symbolTable =
  case HM.lookup text $ symbolTable.getSymbolTable of
    Just n -> Just (Symbol text n)
    Nothing -> Nothing


insert :: Text -> Int -> SymbolTable -> SymbolTable
insert text hash = SymbolTable . HM.insert text hash . (.getSymbolTable)
