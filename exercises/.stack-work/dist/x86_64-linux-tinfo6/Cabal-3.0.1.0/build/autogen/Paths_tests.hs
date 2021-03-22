{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tests (
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
version = Version [0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/felix/haskell-tests/haskell-mooc/exercises/.stack-work/install/x86_64-linux-tinfo6/9341643eb8b12cd9da0ef40f1330623c997b9a8801aac48dbfd772207b3111ff/8.8.4/bin"
libdir     = "/home/felix/haskell-tests/haskell-mooc/exercises/.stack-work/install/x86_64-linux-tinfo6/9341643eb8b12cd9da0ef40f1330623c997b9a8801aac48dbfd772207b3111ff/8.8.4/lib/x86_64-linux-ghc-8.8.4/tests-0.0.1-CPugVscsYV6GzENqUwo5xn"
dynlibdir  = "/home/felix/haskell-tests/haskell-mooc/exercises/.stack-work/install/x86_64-linux-tinfo6/9341643eb8b12cd9da0ef40f1330623c997b9a8801aac48dbfd772207b3111ff/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/felix/haskell-tests/haskell-mooc/exercises/.stack-work/install/x86_64-linux-tinfo6/9341643eb8b12cd9da0ef40f1330623c997b9a8801aac48dbfd772207b3111ff/8.8.4/share/x86_64-linux-ghc-8.8.4/tests-0.0.1"
libexecdir = "/home/felix/haskell-tests/haskell-mooc/exercises/.stack-work/install/x86_64-linux-tinfo6/9341643eb8b12cd9da0ef40f1330623c997b9a8801aac48dbfd772207b3111ff/8.8.4/libexec/x86_64-linux-ghc-8.8.4/tests-0.0.1"
sysconfdir = "/home/felix/haskell-tests/haskell-mooc/exercises/.stack-work/install/x86_64-linux-tinfo6/9341643eb8b12cd9da0ef40f1330623c997b9a8801aac48dbfd772207b3111ff/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tests_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tests_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tests_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tests_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tests_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tests_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
