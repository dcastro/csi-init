module Main where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Process
import System.Environment
import Control.Conditional

import Options.Applicative

-- csi-init -d <bin_dir1> -d <bin_dir2> -- /u:<namespace1>
main :: IO ()
main =
  mkOpts >>= customExecParser parserPrefs >>= prepareArgs >>= callProcess "csi"

prepareArgs :: Flags -> IO [String]
prepareArgs (Flags dirs args) = do
  dlls <- join <$> mapM findDlls dirs
  let importDirs = ("/lib:" ++) <$> dirs
  let importDlls = if null dlls then ""
                    else "/r:" ++ join (intersperse "," dlls)
  return $ importDlls : importDirs ++ args

findDlls :: FilePath -> IO [FilePath]
findDlls dir = filter ((".dll" ==) . takeExtension) . map (makeRelative dir) <$> listFilesRecursively dir

data Flags = Flags {
  asmDirs :: [String],
  args    :: [String]
}

flags :: Parser Flags
flags = Flags
  <$> many ( strOption
    ( long "dir"
    <> short 'd'
    <> metavar "ASSEMBLYDIR"
    <> help "Import all assemblies from the specified folder"))
  <*> many (strArgument (metavar "ARGUMENTS..."))

mkOpts :: IO (ParserInfo Flags)
mkOpts = do
  progName <- getProgName
  return $ info (helper <*> flags)
      ( fullDesc
      <> progDesc ("All arguments specified after `--` will be passed to `csi`. E.g.: `" ++ progName ++ " -- /u:Newtonsoft.Json`")
      <> header "Invokes the `csi` C# REPL preloaded with a bunch of assemblies.")

parserPrefs = prefs showHelpOnError


listFilesRecursively :: FilePath -> IO [String]
listFilesRecursively dir = do
  let go path = ifM (isFile path) (return [path]) (listFilesRecursively path)
  paths <- listDirectoryAbs dir
  all   <- mapM go paths
  return (join all)

-- returns absolute filepaths for all files and directories inside a directory
-- adapted from: http://stackoverflow.com/a/8572250/857807
listDirectoryAbs :: FilePath -> IO [FilePath]
listDirectoryAbs dir =
  sort <$> (listDirectory dir >>= mapM (canonicalizePath . (dir </>)))

isFile = doesFileExist
