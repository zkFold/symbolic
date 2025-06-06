{-# LANGUAGE CPP #-}

import           Control.Exception                  (throwIO)
import           Control.Monad
import           Data.Functor                       (($>))
import           Data.List                          (dropWhile, find, findIndex, isPrefixOf, tails)
import           Data.Maybe                         (fromMaybe)
import           Distribution.PackageDescription    hiding (libName)
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..), localPkgDescr)
import           Distribution.Simple.Program.Find   (defaultProgramSearchPath, findProgramOnSearchPath)
import           Distribution.Simple.Setup
import           Distribution.Types.HookedBuildInfo
import           Distribution.Utils.Path            (unsafeMakeSymbolicPath)
import           Distribution.Verbosity             (Verbosity)
import qualified Distribution.Verbosity             as Verbosity
import           System.Directory
import           System.Exit
import           System.FilePath                    ((</>))
import           System.Process                     (system)

isWasm :: Bool
#if defined(wasm32_HOST_ARCH)
isWasm = True
#else
isWasm = False
#endif

main :: IO ()
main = defaultMainWithHooks hooks
  where

    hooks = simpleUserHooks
      { preConf = \_ _ -> execCargoBuild >> return emptyHookedBuildInfo
      , confHook = \a flags ->
          confHook simpleUserHooks a flags
              >>= rsAddDirs
      }

rsFolder :: FilePath
rsFolder = "rust-wrapper"

libName :: String
libName = "librust_wrapper.a"

execCargoBuild :: IO ()
execCargoBuild = do
    cargoPath <- findProgramOnSearchPath Verbosity.silent defaultProgramSearchPath "cargo"
    let cargoExec = case cargoPath of
            Just (p, _) -> p
            Nothing     -> "cargo"
        suffix = if isWasm then " --target=wasm32-unknown-unknown" else ""
    buildResult <- system $ cargoExec ++ " +nightly build --release --manifest-path rust-wrapper/Cargo.toml -Z unstable-options" ++ suffix

    case buildResult of
      ExitSuccess          -> return ()
      ExitFailure exitCode -> do
        throwIO $ userError $ "Build rust library failed with exit code " <> show exitCode

rsAddDirs :: LocalBuildInfo -> IO LocalBuildInfo
rsAddDirs lbi' = do
    dir <- getCurrentDirectory
    let rustIncludeDir = dir </> rsFolder
        rustLibDir = dir </> rsFolder </> "target/release"

    (includeRustDir, extraLibDir) <- case findIndex (isPrefixOf "dist-newstyle") (tails dir) of
      Just ind -> do
        let pathToDistNewstyle = take ind dir
            pathToRustLib = pathToDistNewstyle ++ "dist-newstyle"
        copyFile (rustLibDir </> libName) (pathToRustLib </> libName)

        return (pathToRustLib, pathToRustLib)
      Nothing -> return (rustLibDir, rustLibDir)

    let updateLbi lbi = lbi{localPkgDescr = updatePkgDescr (localPkgDescr lbi)}
        updatePkgDescr pkgDescr = pkgDescr{library = updateLib <$> library pkgDescr}
        updateLib lib = lib{libBuildInfo = updateLibBi (libBuildInfo lib)}
        updateLibBi libBuild =
          libBuild
            { includeDirs = unsafeMakeSymbolicPath includeRustDir : includeDirs libBuild
            , extraLibDirs = unsafeMakeSymbolicPath extraLibDir : extraLibDirs libBuild
            }

    pure $ updateLbi lbi'
