module Paths_derive_topdown (
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
version = Version [0,0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Song\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Song\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\derive-topdown-0.0.0.1"
datadir    = "C:\\Users\\Song\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\derive-topdown-0.0.0.1"
libexecdir = "C:\\Users\\Song\\AppData\\Roaming\\cabal\\derive-topdown-0.0.0.1"
sysconfdir = "C:\\Users\\Song\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "derive_topdown_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "derive_topdown_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "derive_topdown_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "derive_topdown_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "derive_topdown_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
