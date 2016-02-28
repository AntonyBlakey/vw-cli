{-# LANGUAGE QuasiQuotes         #-}
module Main where

import qualified Commands                as C
import           Control.Monad           (filterM, when, zipWithM_, (<=<))
import           Control.Monad.Extra     (findM)
import           Data.Maybe              (fromJust)
import           Data.Typeable           (Typeable, typeOf, typeRepArgs)
import           Language.English.Plural (plural)
import qualified Model                   as M
import           System.Console.Docopt
import           System.Environment      (getArgs, lookupEnv)
import           System.Exit             (die)
import           Text.Read               (readMaybe)

usageText :: Docopt
usageText = [docopt|
Usage:
  vw-cli releases
  vw-cli images
  vw-cli add-tag <tag-name> <release-name>
  vw-cli remove-tag <tag-name>
  vw-cli create <new-image-name> (32 | 64) <release-name>
  vw-cli copy <image-name> <new-image-name>
  vw-cli delete <image-name>
  vw-cli backup <image-name> <new-backup-name>
  vw-cli restore <image-name> <backup-name>
  vw-cli run <image-name> [--host <host-name>] [--script <script-name>]

Options:
  -h --host <host-name>      the name of the vm to execute the image on
  -s --script <script-name>  the name of the script to execute in the image

Environment Variables:
  VW_CLI_ROOT  the directory containing the releases/, scripts/ and hosts/ directories
|]

main :: IO ()
main = do

  -- Boilerplate for CLI apps

  args <- parseArgsOrExit usageText =<< getArgs

  let hasCommand = isPresent args . command
      getRequiredArg = fromJust . getArg args . argument
      getLongOption = getArg args . longOption
      cmd @@ action = when (hasCommand cmd) action
      requiredEnvVar name = do
        maybeValue <- lookupEnv name
        case maybeValue of
          Nothing -> die $ "The environment variable " ++ name ++ " is not defined."
          Just val -> pure val

  -- Specific to this app, all lazy so not resolved until used in a command

  let root          = M.Root <$> requiredEnvVar "VW_CLI_ROOT"
      image         = fuzzyLookup (getRequiredArg "image-name") =<< M.images =<< root
      release       = fuzzyLookup (getRequiredArg "release-name") =<< M.releases =<< root
      backup        = fuzzyLookup (getRequiredArg "backup-name") =<< M.backups =<< image
      maybeScript   = traverse (\n -> fuzzyLookup n =<< M.scripts =<< root) (getLongOption "script-name")
      maybeHost     = pure $ getLongOption "host-name" -- TODO: hosts should be a type
      newImageName  = uniqueName (getRequiredArg "new-image-name") =<< M.images =<< root
      newBackupName = getRequiredArg "new-backup-name"
      tagName       = getRequiredArg "tag-name"
      width         = if hasCommand "32" then M.Width32 else M.Width64

  "releases"   @@ do r <- root    ; C.listReleases r
  "images"     @@ do r <- root    ; C.listImages r
  "remove-tag" @@ do r <- root    ; C.removeReleaseTag r tagName
  "add-tag"    @@ do r <- release ; C.addReleaseTag r tagName
  "create"     @@ do r <- release ; n <- newImageName ; C.createImage r n width
  "copy"       @@ do i <- image   ; n <- newImageName ; C.copyImage i n
  "delete"     @@ do i <- image   ; C.deleteImage i
  "run"        @@ do i <- image   ; h <- maybeHost    ; s <- maybeScript ; C.runImage i h s
  "backup"     @@ do i <- image   ; C.createBackup i newBackupName
  "restore"    @@ do b <- backup  ; C.restoreBackup b


nameOfListMemberType :: Typeable a => [a] -> String
nameOfListMemberType = show . head . typeRepArgs . typeOf


fuzzyLookup :: M.Nameable a => String -> [a] -> IO a
fuzzyLookup searchTerm list = do

  found <- findM (fmap (elem searchTerm) . M.identifiers) list
  case found of
    Just value -> pure value
    Nothing -> do
      results <- filterM (fmap (any (fuzzyEq searchTerm)) . M.identifiers) list
      case length results of
        0 -> die $ "\"" ++ searchTerm ++ "\" matches no " ++ termName
        1 -> pure $ head results
        _ -> chooseFromList results

  where

    termName = plural $ nameOfListMemberType list

    fuzzyEq :: String -> String -> Bool
    fuzzyEq "*" _ = True
    fuzzyEq "" _  = True
    fuzzyEq _ ""  = False
    fuzzyEq search@(h1 : t1) (h2 : t2) | h1 == h2  = fuzzyEq t1 t2
                                       | otherwise = fuzzyEq search t2

    chooseFromList :: M.Nameable a => [a] -> IO a
    chooseFromList choices = do
      putStrLn $ "Select from the following " ++ termName ++ ":"
      zipWithM_  printChoice [0..] choices
      input <- getLine
      case readMaybe input of
        (Just x) | 0 <= x && x < length choices -> pure $ choices !! x
        _                                       -> chooseFromList choices

    printChoice :: M.Nameable a => Int -> a -> IO ()
    printChoice index = putStrLn . ((show index ++ ") ") ++) . unwords <=< M.identifiers


uniqueName :: M.Nameable a => String -> [a] -> IO String
uniqueName name list =
  if any ((name ==) . M.name) list
    then die $ nameOfListMemberType list ++ " \"" ++ name ++ "\" already exists"
    else pure name
