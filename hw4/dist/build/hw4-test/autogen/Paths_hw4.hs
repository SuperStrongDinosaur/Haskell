{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw4 (
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

bindir     = "/Users/SuperStrongDinosaur/.cabal/bin"
libdir     = "/Users/SuperStrongDinosaur/.cabal/lib/x86_64-osx-ghc-8.4.2/hw4-0.1.0.0-4FPnXwZ4fU4HW8af69evZ-hw4-test"
dynlibdir  = "/Users/SuperStrongDinosaur/.cabal/lib/x86_64-osx-ghc-8.4.2"
datadir    = "/Users/SuperStrongDinosaur/.cabal/share/x86_64-osx-ghc-8.4.2/hw4-0.1.0.0"
libexecdir = "/Users/SuperStrongDinosaur/.cabal/libexec/x86_64-osx-ghc-8.4.2/hw4-0.1.0.0"
sysconfdir = "/Users/SuperStrongDinosaur/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw4_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw4_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
