{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_HaskellTensor (
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

bindir     = "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2/HaskellTensor/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/bin"
libdir     = "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2/HaskellTensor/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/lib/x86_64-osx-ghc-8.4.3/HaskellTensor-0.1.0.0-1vunVB2Vrj1KV1boo6bPul-HaskellTensor"
dynlibdir  = "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2/HaskellTensor/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2/HaskellTensor/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/share/x86_64-osx-ghc-8.4.3/HaskellTensor-0.1.0.0"
libexecdir = "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2/HaskellTensor/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/libexec/x86_64-osx-ghc-8.4.3/HaskellTensor-0.1.0.0"
sysconfdir = "/Users/TobiasReinhart/Desktop/HaskellTensor/HaskellTensor2/HaskellTensor/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskellTensor_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskellTensor_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HaskellTensor_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HaskellTensor_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellTensor_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskellTensor_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
