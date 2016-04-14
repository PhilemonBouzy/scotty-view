module Paths_scotty_view (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Philemon/.cabal/bin"
libdir     = "/Users/Philemon/.cabal/lib/x86_64-osx-ghc-7.8.3/scotty-view-1"
datadir    = "/Users/Philemon/.cabal/share/x86_64-osx-ghc-7.8.3/scotty-view-1"
libexecdir = "/Users/Philemon/.cabal/libexec"
sysconfdir = "/Users/Philemon/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "scotty_view_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "scotty_view_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "scotty_view_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "scotty_view_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "scotty_view_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
