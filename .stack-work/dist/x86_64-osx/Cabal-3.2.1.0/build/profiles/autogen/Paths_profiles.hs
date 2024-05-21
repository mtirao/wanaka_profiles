{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_profiles (
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

bindir     = "/Users/mtirao/Development/Haskell/profiles/.stack-work/install/x86_64-osx/cd334c114a505ffd497328b9c6657c616087b2a11d9bdbf0c6acfd95ec107992/8.10.7/bin"
libdir     = "/Users/mtirao/Development/Haskell/profiles/.stack-work/install/x86_64-osx/cd334c114a505ffd497328b9c6657c616087b2a11d9bdbf0c6acfd95ec107992/8.10.7/lib/x86_64-osx-ghc-8.10.7/profiles-0.1.0.0-JatHjAjYCU5AQapT3slCHT-profiles"
dynlibdir  = "/Users/mtirao/Development/Haskell/profiles/.stack-work/install/x86_64-osx/cd334c114a505ffd497328b9c6657c616087b2a11d9bdbf0c6acfd95ec107992/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/mtirao/Development/Haskell/profiles/.stack-work/install/x86_64-osx/cd334c114a505ffd497328b9c6657c616087b2a11d9bdbf0c6acfd95ec107992/8.10.7/share/x86_64-osx-ghc-8.10.7/profiles-0.1.0.0"
libexecdir = "/Users/mtirao/Development/Haskell/profiles/.stack-work/install/x86_64-osx/cd334c114a505ffd497328b9c6657c616087b2a11d9bdbf0c6acfd95ec107992/8.10.7/libexec/x86_64-osx-ghc-8.10.7/profiles-0.1.0.0"
sysconfdir = "/Users/mtirao/Development/Haskell/profiles/.stack-work/install/x86_64-osx/cd334c114a505ffd497328b9c6657c616087b2a11d9bdbf0c6acfd95ec107992/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "profiles_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "profiles_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "profiles_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "profiles_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "profiles_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "profiles_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
