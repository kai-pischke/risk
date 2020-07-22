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

bindir     = "C:\\Users\\alexd\\Desktop\\risk-master\\risk\\.stack-work\\install\\392054cf\\bin"
libdir     = "C:\\Users\\alexd\\Desktop\\risk-master\\risk\\.stack-work\\install\\392054cf\\lib\\x86_64-windows-ghc-8.8.3\\risk-0.1.0.0-E5B0hAzem1D5d5iZveCTBB-risk-exe"
dynlibdir  = "C:\\Users\\alexd\\Desktop\\risk-master\\risk\\.stack-work\\install\\392054cf\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\alexd\\Desktop\\risk-master\\risk\\.stack-work\\install\\392054cf\\share\\x86_64-windows-ghc-8.8.3\\risk-0.1.0.0"
libexecdir = "C:\\Users\\alexd\\Desktop\\risk-master\\risk\\.stack-work\\install\\392054cf\\libexec\\x86_64-windows-ghc-8.8.3\\risk-0.1.0.0"
sysconfdir = "C:\\Users\\alexd\\Desktop\\risk-master\\risk\\.stack-work\\install\\392054cf\\etc"

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
  return (dir ++ "\\" ++ name)