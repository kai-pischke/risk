{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_risk (
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

bindir     = "/Users/kaipischke/Documents/Fun/haskell/stack_test/project/risk/.stack-work/install/x86_64-osx/73386c5ebd424c7faa4ce3dd616b56f80c57f28705e951dd9d08b377089b129a/8.8.3/bin"
libdir     = "/Users/kaipischke/Documents/Fun/haskell/stack_test/project/risk/.stack-work/install/x86_64-osx/73386c5ebd424c7faa4ce3dd616b56f80c57f28705e951dd9d08b377089b129a/8.8.3/lib/x86_64-osx-ghc-8.8.3/risk-0.1.0.0-LtmN7Mf1fhuJrsPcNZ0HPP"
dynlibdir  = "/Users/kaipischke/Documents/Fun/haskell/stack_test/project/risk/.stack-work/install/x86_64-osx/73386c5ebd424c7faa4ce3dd616b56f80c57f28705e951dd9d08b377089b129a/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/kaipischke/Documents/Fun/haskell/stack_test/project/risk/.stack-work/install/x86_64-osx/73386c5ebd424c7faa4ce3dd616b56f80c57f28705e951dd9d08b377089b129a/8.8.3/share/x86_64-osx-ghc-8.8.3/risk-0.1.0.0"
libexecdir = "/Users/kaipischke/Documents/Fun/haskell/stack_test/project/risk/.stack-work/install/x86_64-osx/73386c5ebd424c7faa4ce3dd616b56f80c57f28705e951dd9d08b377089b129a/8.8.3/libexec/x86_64-osx-ghc-8.8.3/risk-0.1.0.0"
sysconfdir = "/Users/kaipischke/Documents/Fun/haskell/stack_test/project/risk/.stack-work/install/x86_64-osx/73386c5ebd424c7faa4ce3dd616b56f80c57f28705e951dd9d08b377089b129a/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "risk_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "risk_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "risk_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "risk_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "risk_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "risk_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
