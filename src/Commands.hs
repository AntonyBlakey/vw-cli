{-# LANGUAGE OverloadedStrings #-}
module Commands
       ( listReleases
       , listImages
       , addReleaseTag
       , removeReleaseTag
       , createImage
       , copyImage
       , deleteImage
       , createBackup
       , restoreBackup
       , runImage
       ) where

import           Control.Monad             (forM_)
import           Data.List                 (sortOn)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text.IO
import           Data.Time.Clock           (getCurrentTime)
import           Distribution.Simple.Utils (copyDirectoryRecursive,
                                            withTempDirectory)
import           Distribution.Verbosity    (silent)
import qualified Model                     as M
import           System.Directory
import           System.FilePath
import           System.IO                 (IOMode (..), hPutStrLn, withFile)
import           System.Process            (system)
import qualified Table                     as T


listReleases :: M.Root -> IO ()
listReleases root =
  mapM_ putStrLn . T.format . (title :) . T.clearRepeatingCells [True, True] . concat =<< (mapM generateRow =<< (sortOn M.name <$> M.releases root))
  where
    title = T.Title ["Release", "Tags", "Image", "Size"]
    generateRow release = do
      tags <- M.tags release
      let releaseFragment = [M.name release, unwords tags]
      let imageFormatter i = (\width -> [M.name i, if width == M.Width32 then " 32" else " 64"]) <$> M.width i
      imageFragments <- mapM imageFormatter . sortOn M.name =<< M.images release
      pure $ T.Separator : if null imageFragments then [T.Body releaseFragment] else map (T.Body . (releaseFragment ++)) imageFragments


listImages :: M.Root -> IO ()
listImages root =
  mapM_ putStrLn . T.format . (title :) . concat =<< (mapM generateRow =<< (sortOn M.name <$> M.images root))
  where
    title = T.Title ["Image", "Size", "Release"]
    generateRow image = do
      width <- M.width image
      let imageRow = T.Body [M.name image, if width == M.Width32 then " 32" else " 64", M.name $ M.release image]
      backupRows <- map (\x -> T.Body ["      " ++ show (M.timestamp x) ++ " " ++ M.name x]) . sortOn M.timestamp <$> M.backups image
      pure $ if null backupRows then [imageRow] else T.Separator : imageRow : backupRows ++ [T.Separator]


addReleaseTag :: M.Release -> String -> IO ()
addReleaseTag release tagName = do
  removeReleaseTag (M.root release) tagName
  M.addTag release tagName


removeReleaseTag :: M.Root -> String -> IO ()
removeReleaseTag root tagName =
  mapM_ (`M.removeTag` tagName) =<< M.releases root


createImage :: M.Release -> String -> M.Width -> IO ()
createImage release newImageName width = do
  createDirectoryIfMissing True destDir
  copyFile srcFilePath destFilePath
  setPermissions destFilePath =<< setOwnerWritable True <$> getPermissions destFilePath
  where
    srcFilePath = M.path release </> "release" </> "image" </> if width == M.Width32 then "visual.im" else "visual64.im"
    destDir = M.path release </> "images" </> newImageName
    destFilePath = destDir </> if width == M.Width32 then "visual32.im" else "visual64.im"


copyImage :: M.Image -> String -> IO ()
copyImage image newImageName =
  let srcDir = M.path image
      destDir = M.path (M.release image) </> "images" </> newImageName
  in do
    createDirectoryIfMissing True destDir
    copyDirectoryRecursive silent srcDir destDir


deleteImage :: M.Image -> IO ()
deleteImage = removeDirectoryRecursive . M.path


createBackup :: M.Image -> String -> IO ()
createBackup _image _newBackupName = undefined


restoreBackup :: M.Backup -> IO ()
restoreBackup _backup = undefined


runImage :: M.Image -> Maybe String -> Maybe M.Script -> IO ()
runImage image _maybeHost maybeScript = do

  let rootTempDir = M.path (M.root image) </> "temp"
  createDirectoryIfMissing True rootTempDir
  withTempDirectory silent rootTempDir "vw.exec" $ \tempDir -> do

    let release = M.release image

    userScriptCommandTail <- case maybeScript of
      Nothing -> pure ""
      Just script -> do
        let userScriptFileName = tempDir </> "user.st"
        let subst var val = Text.replace var (Text.pack val)
        Text.IO.writeFile userScriptFileName =<<
          subst "{{SYSTEM.SCRIPTS-DIRECTORY}}" (dropFileName $ M.path script) .
          subst "{{RELEASE.DEV-CYCLE}}" (M.devCycle release)
          <$> Text.IO.readFile (M.path script)
        pure $ " -doit 'Compiler evaluate: '\"'" ++ userScriptFileName ++ "'\"' asFilename contentsOfEntireFile.'"

    width <- M.width image
    let executableFilePath = M.visualExecutable release M.OSX width
        imageFileName = M.path image </> if width == M.Width32 then "visual32.im" else "visual64.im"
        innerScriptFileName = tempDir </> "inner.sh"

    withFile innerScriptFileName WriteMode $ \handle -> do
      time <- getCurrentTime
      let header = T.format
            [ T.Body [ "Image", M.name image ]
            , T.Body [ "Width", if width == M.Width32 then "32 bit" else "64 bit" ]
            , T.Body [ "Release", M.name release ]
            , case maybeScript of
                Just script -> T.Body [ "Script", M.name script ]
                _           -> T.Empty
            , T.Body [ "Time", show time ]
            ]
      forM_ header $ hPutStrLn handle . ("echo '" ++) . (++ "'")
      hPutStrLn handle $ "export DISPLAY=:0" -- just for X11
      hPutStrLn handle $ "export VISUALWORKS='" ++ (M.path release </> "release") ++ "'"
      hPutStrLn handle $ executableFilePath ++ " " ++ imageFileName ++ userScriptCommandTail

    _ <- system $ "/bin/bash " ++ innerScriptFileName
    pure ()
