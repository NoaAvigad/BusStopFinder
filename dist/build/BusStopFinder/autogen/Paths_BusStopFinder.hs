{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_BusStopFinder (
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

bindir     = "/Users/Noa/.cabal/bin"
libdir     = "/Users/Noa/.cabal/lib/x86_64-osx-ghc-8.2.1/BusStopFinder-0.1.0.0-IMtusY3fC0c8QiJa5WhaTQ"
dynlibdir  = "/Users/Noa/.cabal/lib/x86_64-osx-ghc-8.2.1"
datadir    = "/Users/Noa/.cabal/share/x86_64-osx-ghc-8.2.1/BusStopFinder-0.1.0.0"
libexecdir = "/Users/Noa/.cabal/libexec/x86_64-osx-ghc-8.2.1/BusStopFinder-0.1.0.0"
sysconfdir = "/Users/Noa/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "BusStopFinder_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "BusStopFinder_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "BusStopFinder_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "BusStopFinder_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "BusStopFinder_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "BusStopFinder_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
