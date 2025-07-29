{-# LANGUAGE CPP #-}

import Control.Exception (throwIO)
import Control.Monad
import Data.Functor (($>))
import Data.List (
  dropWhile,
  find,
  findIndex,
  isPrefixOf,
  tails,
 )
import Data.Maybe (fromMaybe)
import Distribution.PackageDescription hiding (libName)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo (..),
  localPkgDescr,
 )
import Distribution.Simple.Program.Find (
  defaultProgramSearchPath,
  findProgramOnSearchPath,
 )
import Distribution.Simple.Setup
import Distribution.Types.HookedBuildInfo
import Distribution.Verbosity (Verbosity)
import qualified Distribution.Verbosity as Verbosity
import System.Directory
import System.Exit
import System.FilePath
import System.Info (os)
import System.Process (system)

#if MIN_VERSION_Cabal(3,14,0)
import qualified Distribution.Utils.Path            as UtilsPath
#endif

type StaticLibPath = FilePath

type DynamicLibPath = FilePath

rsSourceDir :: FilePath
rsSourceDir = "rust-wrapper"

defaultRustOuputPath :: FilePath
defaultRustOuputPath = rsSourceDir </> "target" </> "release"

originalStaticLibName :: String
originalStaticLibName = "librust_wrapper.a"

originalDynamicLibName :: String
originalDynamicLibName = "librust_wrapper"

staticLibName :: String
staticLibName = "librust_wrapper_stat.a"

dynamicLibName :: String
dynamicLibName = "librust_wrapper_dyn"

dynExt :: String
dynExt = case os of
  "darwin" -> "dylib"
  "linux" -> "so"
  _anyOther ->
    error $
      "Unsupported operating system: "
        ++ os
        ++ ". Please raise an issue at https://github.com/zkFold/symbolic to request support for this platform."

main :: IO ()
main = defaultMainWithHooks hooks
 where
  hooks =
    simpleUserHooks
      { preConf = \a b -> execCargoBuild >> preConf simpleUserHooks a b
      , confHook = \args flags -> do
          dir <- getCurrentDirectory
          let pathToDistNewstyle = joinPath . reverse . dropWhile (not . isPrefixOf "dist-") . reverse . splitPath $ dir
              absoluteRustOutputPath = dir </> defaultRustOuputPath

          path <-
            if null pathToDistNewstyle
              then return absoluteRustOutputPath
              else do
                copyVerbose (absoluteRustOutputPath </> staticLibName) (pathToDistNewstyle </> staticLibName)
                copyVerbose (absoluteRustOutputPath </> dynamicLibName <.> dynExt) (pathToDistNewstyle </> dynamicLibName <.> dynExt)
                return pathToDistNewstyle

          addExtraLibDir path <$> confHook simpleUserHooks args flags
      , preBuild = \args flags -> do
          return (Just $ emptyBuildInfo {extraLibs = ["rust_wrapper_stat"]}, [])
      , preReg = \args flags -> do
          return (Just $ emptyBuildInfo {extraLibs = ["rust_wrapper_stat"]}, [])
      , preRepl = \args flags -> do
          return (Just $ emptyBuildInfo {extraLibs = ["rust_wrapper_dyn"]}, [])
      }

copyVerbose :: FilePath -> FilePath -> IO ()
copyVerbose origin destination = do
    putStrLn $ unlines ["Copying", origin, "to", destination]
    copyFile origin destination

renameVerbose :: FilePath -> FilePath -> IO ()
renameVerbose origin destination = do
    putStrLn $ unlines ["Renaming", origin, "to", destination]
    renameFile origin destination

addExtraLibDir :: FilePath -> LocalBuildInfo -> LocalBuildInfo
addExtraLibDir extraLibDir lbi = lbi {localPkgDescr = updatePkgDescr (localPkgDescr lbi)}
 where
  updatePkgDescr pkgDescr = pkgDescr {library = updateLib <$> library pkgDescr}
  updateLib lib = lib {libBuildInfo = updateBi (libBuildInfo lib)}
  updateBi bi = bi {extraLibDirs = mkSymbolicPath extraLibDir : extraLibDirs bi}

runCargoOrThrow :: String -> IO ()
runCargoOrThrow cargoArgs = do
  cargoPath <- findProgramOnSearchPath Verbosity.silent defaultProgramSearchPath "cargo"

  let cargo = case cargoPath of
        Just (p, _) -> p
        Nothing -> "cargo"
      cargoRun = cargo <> " " <> cargoArgs

  buildResult <- system cargoRun

  case buildResult of
    ExitSuccess -> return ()
    ExitFailure exitCode -> do
      throwIO $ userError $ "'" <> cargoRun <> "' failed with exit code " <> show exitCode

execCargoBuild :: IO ()
execCargoBuild = do
  runCargoOrThrow $ "build --release --manifest-path " <> rsSourceDir </> "Cargo.toml"
  renameVerbose
    (defaultRustOuputPath </> originalDynamicLibName <.> dynExt)
    (defaultRustOuputPath </> dynamicLibName <.> dynExt)
  renameVerbose (defaultRustOuputPath </> originalStaticLibName) (defaultRustOuputPath </> staticLibName)

#if MIN_VERSION_Cabal(3,14,0)
mkSymbolicPath = UtilsPath.unsafeMakeSymbolicPath
#else
mkSymbolicPath = id
#endif
