{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_hasu (
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

bindir     = "C:\\Users\\minec\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\minec\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.4\\hasu-0.1.0.0-inplace-hasu"
dynlibdir  = "C:\\Users\\minec\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\minec\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.4\\hasu-0.1.0.0"
libexecdir = "C:\\Users\\minec\\AppData\\Roaming\\cabal\\hasu-0.1.0.0-inplace-hasu\\x86_64-windows-ghc-8.10.4\\hasu-0.1.0.0"
sysconfdir = "C:\\Users\\minec\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hasu_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hasu_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hasu_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hasu_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hasu_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hasu_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
