{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Process
import System.Environment
import Control.Conditional hiding (when)
import Options.Applicative
import Control.Newtype

{-# ANN findDlls            "HLint: ignore Use fmap" #-}
{-# ANN findDllsRecursively "HLint: ignore Use fmap" #-}

newtype Directory = Directory String
newtype Argument  = Argument  String

instance Newtype Directory String where
  pack = Directory
  unpack (Directory s) = s

instance Newtype Argument String where
    pack = Argument
    unpack (Argument s) = s

-- csi-init -d <bin_dir1> -d <bin_dir2> -- /u:<namespace1>
main :: IO ()
main = do
  fs <- mkOpts >>= customExecParser parserPrefs
  args <- prepareArgs fs
  printArgs fs args
  callProcess "csi" (unpack <$> args)

prepareArgs :: Flags -> IO [Argument]
prepareArgs (Flags dirs rdirs args _) = do
  dlls <- join <$> liftM2 (++) (mapM findDlls dirs) (mapM findDllsRecursively rdirs)
  let importDirs = over Directory ("/lib:" ++) <$> (dirs ++ rdirs)
  let importDlls = if null dlls then []
                    else [pack $ "/r:" ++ join (intersperse "," dlls)]
  return $ importDirs ++ importDlls ++ args

printArgs :: Flags -> [Argument] -> IO ()
printArgs fs args = when (debug fs) (putStrLn ("Arg count: " ++ show (length args)) >> mapM_ (putStrLn . unpack) args)

findDlls :: Directory -> IO [FilePath]
findDlls dir =
  listDirectoryAbs dir
  >>= filterM isFile
  >>= return . filterDlls
  >>= return . map (makeRelative $ unpack dir)

findDllsRecursively :: Directory -> IO [FilePath]
findDllsRecursively dir =
  listFilesRecursively dir
  >>= return . filterDlls
  >>= return . map (makeRelative $ unpack dir)

filterDlls = filter ((".dll" ==) . takeExtension)

data Flags = Flags {
  asmDirs   :: [Directory],
  asmRDirs  :: [Directory],
  args      :: [Argument],
  debug     :: Bool
}

flags :: Parser Flags
flags = Flags
  <$> many (pack <$> strOption
    ( long "dir"
    <> short 'd'
    <> metavar "ASSEMBLYDIR"
    <> help "Import all assemblies from the specified folder"))
  <*> many (pack <$> strOption
    ( long "rdir"
    <> short 'r'
    <> metavar "ASSEMBLYDIR"
    <> help "Import all assemblies from the specified folder and it's subfolders (recursive search)"))
  <*> many (pack <$> strArgument (metavar "ARGUMENTS..."))
  <*> switch
    ( long "debug"
    <> help "Print the arguments being passed to `csi`")

mkOpts :: IO (ParserInfo Flags)
mkOpts = do
  progName <- getProgName
  return $ info (helper <*> flags)
      ( fullDesc
      <> progDesc ("All arguments specified after `--` will be passed directly to `csi`. E.g.: `" ++ progName ++ " -- /u:Newtonsoft.Json`")
      <> header "Invokes the `csi` C# REPL preloaded with a bunch of assemblies.")

parserPrefs = prefs showHelpOnError

-- returns absolute filepaths for all files inside a directory and its subdirectories
listFilesRecursively :: Directory -> IO [FilePath]
listFilesRecursively dir = do
  let go path = ifM (isFile path) (return [path]) (listFilesRecursively $ pack path)
  paths <- listDirectoryAbs dir
  all   <- mapM go paths
  return (join all)

-- returns absolute filepaths for all files and directories inside a directory
-- adapted from: http://stackoverflow.com/a/8572250/857807
listDirectoryAbs :: Directory -> IO [FilePath]
listDirectoryAbs (Directory dir) =
  sort <$> (listDirectory dir >>= mapM (canonicalizePath . (dir </>)))

isFile = doesFileExist
