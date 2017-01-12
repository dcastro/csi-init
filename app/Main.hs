{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Control.Monad
import Data.List hiding (find)
import System.Process
import System.Environment
import Options.Applicative
import Control.Newtype
import System.FilePath.Find hiding (Directory)

newtype Directory = Directory String deriving Show
newtype Argument  = Argument  String deriving Show

instance Newtype Directory String where
  pack = Directory
  unpack (Directory s) = s

instance Newtype Argument String where
    pack = Argument
    unpack (Argument s) = s

-- csi-init -d <bin_dir1> -d <bin_dir2> -- /u:<namespace1>
main :: IO ()
main = do
  (Flags dirs rdirs args debug) <- mkOpts >>= customExecParser parserPrefs
  dlls <- findFiles dirs rdirs isDll
  let args' = prepareArgs (dirs ++ rdirs) dlls args
  when debug (printArgs args')
  callProcess "csi" (unpack <$> args')

findFiles :: [Directory] -> [Directory] -> FilterPredicate -> IO [FilePath]
findFiles dirs rdirs pred =
  join <$> sequence (
    (dirs  <&> (find (depth <? 1) pred . unpack)) ++
    (rdirs <&> (find always       pred . unpack))
  )

isDll :: FilterPredicate
isDll = extension ==? ".dll" &&? fileType ==? RegularFile

prepareArgs :: [Directory] -> [FilePath] -> [Argument] -> [Argument]
prepareArgs dirs dlls args =
  let importDirs = over Directory ("/lib:" ++) <$> dirs
      importDlls = if null dlls then []
                    else [pack $ "/r:" ++ join (intersperse "," dlls)]
  in  importDirs ++ importDlls ++ args

printArgs :: [Argument] -> IO ()
printArgs args = putStrLn ("Arg count: " ++ show (length args)) >> mapM_ (putStrLn . unpack) args

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

-- flipped fmap
infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
