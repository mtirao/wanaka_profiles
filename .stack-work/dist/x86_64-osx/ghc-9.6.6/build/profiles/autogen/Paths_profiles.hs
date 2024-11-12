{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_profiles (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/mtirao/Development/haskell/wanaka_profiles/.stack-work/install/x86_64-osx/26e707bb1d0eddb49392d2bead29e5edd80f18d172e242f5e42939ad426503d7/9.6.6/bin"
libdir     = "/Users/mtirao/Development/haskell/wanaka_profiles/.stack-work/install/x86_64-osx/26e707bb1d0eddb49392d2bead29e5edd80f18d172e242f5e42939ad426503d7/9.6.6/lib/x86_64-osx-ghc-9.6.6/profiles-0.1.0.0-Dq4EgfXbitTGzOqDucPkC0-profiles"
dynlibdir  = "/Users/mtirao/Development/haskell/wanaka_profiles/.stack-work/install/x86_64-osx/26e707bb1d0eddb49392d2bead29e5edd80f18d172e242f5e42939ad426503d7/9.6.6/lib/x86_64-osx-ghc-9.6.6"
datadir    = "/Users/mtirao/Development/haskell/wanaka_profiles/.stack-work/install/x86_64-osx/26e707bb1d0eddb49392d2bead29e5edd80f18d172e242f5e42939ad426503d7/9.6.6/share/x86_64-osx-ghc-9.6.6/profiles-0.1.0.0"
libexecdir = "/Users/mtirao/Development/haskell/wanaka_profiles/.stack-work/install/x86_64-osx/26e707bb1d0eddb49392d2bead29e5edd80f18d172e242f5e42939ad426503d7/9.6.6/libexec/x86_64-osx-ghc-9.6.6/profiles-0.1.0.0"
sysconfdir = "/Users/mtirao/Development/haskell/wanaka_profiles/.stack-work/install/x86_64-osx/26e707bb1d0eddb49392d2bead29e5edd80f18d172e242f5e42939ad426503d7/9.6.6/etc"

getBinDir     = catchIO (getEnv "profiles_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "profiles_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "profiles_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "profiles_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "profiles_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "profiles_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
