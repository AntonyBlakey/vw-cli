{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Commands                as C
import           Control.Monad           (filterM, when, zipWithM)
import           Control.Monad.Extra     (findM)
import           Data.Maybe              (fromJust)
import           Data.Typeable           (Typeable, typeOf, typeRepArgs)
import           Language.English.Plural (plural)
import qualified Model                   as M
import           System.Console.Docopt
import           System.Environment      (getArgs, lookupEnv)
import           System.Exit             (die)
import           System.IO               (hFlush, stdout)
import qualified Table                   as T
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
        value <- lookupEnv name
        case value of
          Just val -> pure val
          Nothing -> die $ "The environment variable " ++ name ++ " is not defined."

  -- Specific to this app, all lazy so not resolved until used in a command

  let root          = M.Root <$> requiredEnvVar "VW_CLI_ROOT"
      image         = fuzzyLookup (getRequiredArg "image-name") =<< M.images =<< root
      release       = fuzzyLookup (getRequiredArg "release-name") =<< M.releases =<< root
      backup        = fuzzyLookup (getRequiredArg "backup-name") =<< M.backups =<< image
      maybeScript   = traverse (\n -> fuzzyLookup n =<< M.scripts =<< root) (getLongOption "script")
      maybeHost     = pure $ getLongOption "host" -- TODO: hosts should be a type
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
        0 -> die $ "\"" ++ searchTerm ++ "\" matches no " ++ plural termName
        1 -> pure $ head results
        _ -> chooseFromList results

  where

    termName = nameOfListMemberType list

    fuzzyEq :: String -> String -> Bool
    fuzzyEq "*" _ = True
    fuzzyEq "" _  = True
    fuzzyEq _ ""  = False
    fuzzyEq search@(h1 : t1) (h2 : t2) | h1 == h2  = fuzzyEq t1 t2
                                       | otherwise = fuzzyEq search t2

    chooseFromList :: M.Nameable a => [a] -> IO a
    chooseFromList choices = do
      mapM_ putStrLn =<< T.format <$> zipWithM choiceRow [1..] choices
      putStr $ "Select " ++ termName ++ " number (1 .. " ++ show (length choices) ++ ") or 0 to quit : "
      hFlush stdout
      input <- getLine
      case readMaybe input of
        (Just x) | x == 0                       -> die ""
                 | 0 < x && x <= length choices -> pure $ choices !! (x - 1)
        _                                       -> chooseFromList choices

    choiceRow :: M.Nameable a => Int -> a -> IO T.Row
    choiceRow index = fmap (\ids -> T.Body [ show index, unwords ids ]) . M.identifiers


uniqueName :: M.Nameable a => String -> [a] -> IO String
uniqueName name list =
  if any ((name ==) . M.name) list
    then die $ nameOfListMemberType list ++ " \"" ++ name ++ "\" already exists"
    else pure name
