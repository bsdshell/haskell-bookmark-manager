{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskell_bookmark_manager (
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

bindir     = "/Users/aaa/myfile/github/haskell-bookmark-manager/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/bin"
libdir     = "/Users/aaa/myfile/github/haskell-bookmark-manager/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/lib/x86_64-osx-ghc-8.6.5/haskell-bookmark-manager-0.1.0.0-AgI2XSWpNAKFbMPP0SFdJM-haskell-bookmark-manager"
dynlibdir  = "/Users/aaa/myfile/github/haskell-bookmark-manager/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/aaa/myfile/github/haskell-bookmark-manager/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/share/x86_64-osx-ghc-8.6.5/haskell-bookmark-manager-0.1.0.0"
libexecdir = "/Users/aaa/myfile/github/haskell-bookmark-manager/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/libexec/x86_64-osx-ghc-8.6.5/haskell-bookmark-manager-0.1.0.0"
sysconfdir = "/Users/aaa/myfile/github/haskell-bookmark-manager/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_bookmark_manager_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_bookmark_manager_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_bookmark_manager_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_bookmark_manager_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_bookmark_manager_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_bookmark_manager_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
