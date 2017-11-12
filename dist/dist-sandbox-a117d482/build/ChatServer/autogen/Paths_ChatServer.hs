{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ChatServer (
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

bindir     = "C:\\Users\\diarm\\IdeaProjects\\ChatServer\\.cabal-sandbox\\bin"
libdir     = "C:\\Users\\diarm\\IdeaProjects\\ChatServer\\.cabal-sandbox\\x86_64-windows-ghc-8.2.1\\ChatServer-0.1.0.0-I7LJzhEVoAY7jb523Kzp1o"
dynlibdir  = "C:\\Users\\diarm\\IdeaProjects\\ChatServer\\.cabal-sandbox\\x86_64-windows-ghc-8.2.1"
datadir    = "C:\\Users\\diarm\\IdeaProjects\\ChatServer\\.cabal-sandbox\\x86_64-windows-ghc-8.2.1\\ChatServer-0.1.0.0"
libexecdir = "C:\\Users\\diarm\\IdeaProjects\\ChatServer\\.cabal-sandbox\\ChatServer-0.1.0.0-I7LJzhEVoAY7jb523Kzp1o\\x86_64-windows-ghc-8.2.1\\ChatServer-0.1.0.0"
sysconfdir = "C:\\Users\\diarm\\IdeaProjects\\ChatServer\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ChatServer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ChatServer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ChatServer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ChatServer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ChatServer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ChatServer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
