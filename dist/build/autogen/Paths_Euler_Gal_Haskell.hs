module Paths_Euler_Gal_Haskell (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/nathanedwards/Library/Haskell/ghc-7.4.1/lib/Euler-Gal-Haskell-0.0.1/bin"
libdir     = "/Users/nathanedwards/Library/Haskell/ghc-7.4.1/lib/Euler-Gal-Haskell-0.0.1/lib"
datadir    = "/Users/nathanedwards/Library/Haskell/ghc-7.4.1/lib/Euler-Gal-Haskell-0.0.1/share"
libexecdir = "/Users/nathanedwards/Library/Haskell/ghc-7.4.1/lib/Euler-Gal-Haskell-0.0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Euler_Gal_Haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Euler_Gal_Haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Euler_Gal_Haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Euler_Gal_Haskell_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
