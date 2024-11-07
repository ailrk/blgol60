module Monad where

import Symbol (HasSymbolTable(..), SymbolTable, newSymbolTable)
import UnliftIO (MonadUnliftIO)


data Context = Context
  { symbolTableRef :: IORef SymbolTable
  , tempCounter :: IORef Int
  }


newContext :: MonadUnliftIO m => m Context
newContext = do
  symbolTableRef <- newIORef newSymbolTable
  tempCounter <- newIORef 0
  pure Context {..}


instance HasSymbolTable Context where
  getSymbolTableRef ctx = ctx.symbolTableRef


newtype Algol60 a = Algol60 (ReaderT Context IO a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader Context
  , MonadFail
  , MonadIO
  , MonadUnliftIO)


runAlgol60 :: Algol60 a -> ReaderT Context IO a
runAlgol60 (Algol60 m) = m
