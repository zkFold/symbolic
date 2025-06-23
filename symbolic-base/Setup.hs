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
import           Distribution.Verbosity             (Verbosity)
import qualified Distribution.Verbosity             as Verbosity
import           System.Directory
import           System.Exit
import           System.FilePath                    ((</>))
import           System.Process                     (system)

#if MIN_VERSION_Cabal(3,14,0)
import qualified Distribution.Utils.Path as UtilsPath
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
    buildResult <- system $ cargoExec ++ " +nightly build --release --manifest-path rust-wrapper/Cargo.toml -Z unstable-options"

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
        updatePkgDescr pkgDescr =
            pkgDescr{ library = updateLib <$> library pkgDescr
                    , executables = updateExe <$> executables pkgDescr
                    , benchmarks = updateBench <$> benchmarks pkgDescr
                    , testSuites = updateTests <$> testSuites pkgDescr}

        updateLib lib = lib{libBuildInfo = updateBi (libBuildInfo lib)}
        updateExe exe = exe{buildInfo = updateBi (buildInfo exe)}
        updateBench bench = bench{benchmarkBuildInfo = updateBi (benchmarkBuildInfo bench)}
        updateTests test = test{testBuildInfo = updateBi (testBuildInfo test)}

        updateBi bi =
          bi
            { includeDirs = mkSymbolicPath includeRustDir : includeDirs bi
            , extraLibDirs = mkSymbolicPath extraLibDir : extraLibDirs bi
            , extraLibs = ["rust_wrapper"]
            }

    pure $ updateLbi lbi'

#if MIN_VERSION_Cabal(3,14,0)
mkSymbolicPath = UtilsPath.unsafeMakeSymbolicPath
#else
mkSymbolicPath = id
#endif