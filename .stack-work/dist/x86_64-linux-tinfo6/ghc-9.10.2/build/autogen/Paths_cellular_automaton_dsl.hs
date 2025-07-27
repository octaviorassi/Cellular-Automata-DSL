{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_cellular_automaton_dsl (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/octavio/materias/alp/cellular-automaton-dsl/.stack-work/install/x86_64-linux-tinfo6/02433522259868bf0ef32df8828c0336e52d4e374b4de83bec9a56bf3125a2e0/9.10.2/bin"
libdir     = "/home/octavio/materias/alp/cellular-automaton-dsl/.stack-work/install/x86_64-linux-tinfo6/02433522259868bf0ef32df8828c0336e52d4e374b4de83bec9a56bf3125a2e0/9.10.2/lib/x86_64-linux-ghc-9.10.2-2f07/cellular-automaton-dsl-0.1.0.0-B395Lqkagr76k2gMpcByWl"
dynlibdir  = "/home/octavio/materias/alp/cellular-automaton-dsl/.stack-work/install/x86_64-linux-tinfo6/02433522259868bf0ef32df8828c0336e52d4e374b4de83bec9a56bf3125a2e0/9.10.2/lib/x86_64-linux-ghc-9.10.2-2f07"
datadir    = "/home/octavio/materias/alp/cellular-automaton-dsl/.stack-work/install/x86_64-linux-tinfo6/02433522259868bf0ef32df8828c0336e52d4e374b4de83bec9a56bf3125a2e0/9.10.2/share/x86_64-linux-ghc-9.10.2-2f07/cellular-automaton-dsl-0.1.0.0"
libexecdir = "/home/octavio/materias/alp/cellular-automaton-dsl/.stack-work/install/x86_64-linux-tinfo6/02433522259868bf0ef32df8828c0336e52d4e374b4de83bec9a56bf3125a2e0/9.10.2/libexec/x86_64-linux-ghc-9.10.2-2f07/cellular-automaton-dsl-0.1.0.0"
sysconfdir = "/home/octavio/materias/alp/cellular-automaton-dsl/.stack-work/install/x86_64-linux-tinfo6/02433522259868bf0ef32df8828c0336e52d4e374b4de83bec9a56bf3125a2e0/9.10.2/etc"

getBinDir     = catchIO (getEnv "cellular_automaton_dsl_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "cellular_automaton_dsl_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "cellular_automaton_dsl_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "cellular_automaton_dsl_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cellular_automaton_dsl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cellular_automaton_dsl_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
