module Paths_FingerExercise (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/lingxiao/Documents/Haskell/ExerciseOne--lingxiao/bin"
libdir     = "/Users/lingxiao/Documents/Haskell/ExerciseOne--lingxiao/lib"
datadir    = "/Users/lingxiao/Documents/Haskell/ExerciseOne--lingxiao/share"
libexecdir = "/Users/lingxiao/Documents/Haskell/ExerciseOne--lingxiao/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "FingerExercise_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FingerExercise_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "FingerExercise_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FingerExercise_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
