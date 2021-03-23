{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_blgo60 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jimmy/.cabal/bin"
libdir     = "/home/jimmy/.cabal/lib/x86_64-linux-ghc-8.10.3/blgo60-0.1.0.0-inplace"
dynlibdir  = "/home/jimmy/.cabal/lib/x86_64-linux-ghc-8.10.3"
datadir    = "/home/jimmy/.cabal/share/x86_64-linux-ghc-8.10.3/blgo60-0.1.0.0"
libexecdir = "/home/jimmy/.cabal/libexec/x86_64-linux-ghc-8.10.3/blgo60-0.1.0.0"
sysconfdir = "/home/jimmy/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "blgo60_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "blgo60_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "blgo60_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "blgo60_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "blgo60_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "blgo60_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
