{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_criterion_scraper (
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

bindir     = "/Users/peter/Code/haskell-beginners/criterion-scraper/.stack-work/install/x86_64-osx/30505965a82d31ebedb2d2281a714e83d760d59d20218a59e159c63beea20c7b/8.8.3/bin"
libdir     = "/Users/peter/Code/haskell-beginners/criterion-scraper/.stack-work/install/x86_64-osx/30505965a82d31ebedb2d2281a714e83d760d59d20218a59e159c63beea20c7b/8.8.3/lib/x86_64-osx-ghc-8.8.3/criterion-scraper-0.1.0.0-9FOjqgB3Dfd7UbbExEjBKw-criterion-scraper-exe"
dynlibdir  = "/Users/peter/Code/haskell-beginners/criterion-scraper/.stack-work/install/x86_64-osx/30505965a82d31ebedb2d2281a714e83d760d59d20218a59e159c63beea20c7b/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/peter/Code/haskell-beginners/criterion-scraper/.stack-work/install/x86_64-osx/30505965a82d31ebedb2d2281a714e83d760d59d20218a59e159c63beea20c7b/8.8.3/share/x86_64-osx-ghc-8.8.3/criterion-scraper-0.1.0.0"
libexecdir = "/Users/peter/Code/haskell-beginners/criterion-scraper/.stack-work/install/x86_64-osx/30505965a82d31ebedb2d2281a714e83d760d59d20218a59e159c63beea20c7b/8.8.3/libexec/x86_64-osx-ghc-8.8.3/criterion-scraper-0.1.0.0"
sysconfdir = "/Users/peter/Code/haskell-beginners/criterion-scraper/.stack-work/install/x86_64-osx/30505965a82d31ebedb2d2281a714e83d760d59d20218a59e159c63beea20c7b/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "criterion_scraper_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "criterion_scraper_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "criterion_scraper_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "criterion_scraper_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "criterion_scraper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "criterion_scraper_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
