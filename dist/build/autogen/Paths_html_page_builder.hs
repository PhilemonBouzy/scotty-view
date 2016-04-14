module Paths_html_page_builder (
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
libdir     = "/Users/Philemon/.cabal/lib/x86_64-osx-ghc-7.8.3/html-page-builder-1"
datadir    = "/Users/Philemon/.cabal/share/x86_64-osx-ghc-7.8.3/html-page-builder-1"
libexecdir = "/Users/Philemon/.cabal/libexec"
sysconfdir = "/Users/Philemon/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "html_page_builder_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "html_page_builder_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "html_page_builder_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "html_page_builder_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "html_page_builder_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
