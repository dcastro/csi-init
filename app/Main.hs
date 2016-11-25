module Main where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Process
import Control.Conditional

import Options.Applicative

-- csi-init -d <bin_dir1> -d <bin_dir2> -- /u:<namespace1>
main :: IO ()
main =
  customExecParser parserPrefs opts >>= prepareCmd >>= callProcess "csi"
  where
    prepareCmd (Flags dirs args) = do
      dlls <- filter ((".dll" ==) . takeExtension) . join <$> mapM listFilesRecursively dirs
      return $ ("/r:"++) <$> dlls <|> args

data Flags = Flags {
  asmDirs :: [String],
  args    :: [String]
} deriving (Show)

flags :: Parser Flags
flags = Flags
  <$> many ( strOption
    ( long "dir"
    <> short 'd'
    <> metavar "ASSEMBLYDIR"
    <> help "Import all assemblies from the specified folder"))
  <*> many (strArgument (metavar "ARGUMENTS..."))

opts = info (helper <*> flags)
  ( fullDesc
  <> progDesc "All arguments specified after `--` will be passed to `csi`. E.g.: `csi-init -- /u:Newtonsoft.Json`"
  <> header "Invokes the `csi` C# REPL preloaded with a bunch of assemblies.")

parserPrefs = prefs showHelpOnError

listFilesRecursively :: FilePath -> IO [String]
listFilesRecursively dir =
  let go path = ifM (isFile path) (return [path]) (listFilesRecursively path)
  in do
    paths <- listDirectoryAbs dir
    all <- mapM go paths
    return (join all)

-- returns absolute filepaths for all files and directories inside a directory
-- adapted from: http://stackoverflow.com/a/8572250/857807
listDirectoryAbs :: FilePath -> IO [FilePath]
listDirectoryAbs dir =
  sort <$> (listDirectory dir >>= mapM (canonicalizePath . (dir </>)))

isFile = doesFileExist
