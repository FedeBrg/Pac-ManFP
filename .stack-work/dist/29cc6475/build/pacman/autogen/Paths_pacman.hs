{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_pacman (
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

bindir     = "C:\\Users\\FedericoBergagna\\Desktop\\sanitized_pacman\\.stack-work\\install\\d5de694d\\bin"
libdir     = "C:\\Users\\FedericoBergagna\\Desktop\\sanitized_pacman\\.stack-work\\install\\d5de694d\\lib\\x86_64-windows-ghc-8.8.4\\pacman-0.1.0.0-GiqU9MHJIzeHXbIJFNSpWV-pacman"
dynlibdir  = "C:\\Users\\FedericoBergagna\\Desktop\\sanitized_pacman\\.stack-work\\install\\d5de694d\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\FedericoBergagna\\Desktop\\sanitized_pacman\\.stack-work\\install\\d5de694d\\share\\x86_64-windows-ghc-8.8.4\\pacman-0.1.0.0"
libexecdir = "C:\\Users\\FedericoBergagna\\Desktop\\sanitized_pacman\\.stack-work\\install\\d5de694d\\libexec\\x86_64-windows-ghc-8.8.4\\pacman-0.1.0.0"
sysconfdir = "C:\\Users\\FedericoBergagna\\Desktop\\sanitized_pacman\\.stack-work\\install\\d5de694d\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pacman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pacman_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pacman_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pacman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pacman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pacman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
