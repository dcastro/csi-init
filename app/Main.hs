module Main where

import Lib
import System.Environment
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Process
import Control.Conditional
import System.Exit hiding (die)

import Data.Char
import System.Console.GetOpt
import System.IO
import Text.Printf

data Flags = Asm {asm :: String} | Using { ns :: String } | AsmDir { dir:: String } | Help
  deriving (Eq,Ord,Show)

flags =
  [
    Option ['a'] ["asm"]    (ReqArg Asm "ASSEMBLY")           "Import the specified assembly"
  , Option ['d'] ["dir"]    (ReqArg AsmDir "ASSEMBLY-FOLDER") "Import all assemblies from the specified folder"
  , Option ['u'] ["using"]  (ReqArg Using "USING")            "Import the specified namespace"
  , Option ['h'] ["help"]   (NoArg Help)                      "Print this help message"
  ]

parse args = case getOpt Permute flags args of
  (args, _, []) -> if Help `elem` args then help
                      else foldM f ([], []) args
  (_, _, errs)  -> putErr (join errs) >> putErr (usageInfo header flags) >> exitWith (ExitFailure 1)
  where
    header = "Usage: csi-init [options]"
    help = putErr (usageInfo header flags) >> exitSuccess
    f (asms, nss) (Using ns)    = return (asms, ns : nss)
    f (asms, nss) (Asm asm)     = return (asm : asms, nss)
    f (asms, nss) (AsmDir dir)  = (\xs -> (asms ++ xs, nss)) <$> files
      where files = filter ((".dll" ==) . takeExtension) <$> listFilesRecursively dir

putErr = hPutStrLn stderr

main :: IO ()
main = getArgs >>= parseArgs >>= listFilesRecursively >>= void . run . prepareArgs

-- TODO: pass a list of namespaces
parseArgs ["-v"]  = usage >> exit
parseArgs [dir]   = return dir
parseArgs _       = usage >> exit

usage   = putStrLn "Usage: csi-init <bin-folder>"
exit    = exitSuccess

prepareArgs :: t -> t
prepareArgs dlls = dlls

run args = createProcess (proc "csi" args)

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
