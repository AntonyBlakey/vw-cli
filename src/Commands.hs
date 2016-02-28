{-# LANGUAGE TupleSections #-}
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

import           Data.List                 (sortOn)
import           Distribution.Simple.Utils (copyDirectoryRecursive)
import           Distribution.Verbosity    (silent)
import qualified Model                     as M
import           System.Directory
import           System.FilePath
import           System.Process            (system)
import qualified Table                     as T


listReleases :: M.Root -> IO ()
listReleases root =
  T.print =<< (title :) . T.clearRepeatingCells [True, True] . concat <$> (mapM generateRow =<< (sortOn M.name <$> M.releases root))
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
  T.print =<< (title :) . concat <$> (mapM generateRow =<< (sortOn M.name <$> M.images root))
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
createImage release newImageName width =
  let srcFilePath = M.path release </> "M.release" </> "image" </> if width == M.Width32 then "visual.im" else "visual64.im"
      destDir = M.path release </> "images" </> newImageName
      destFilePath = destDir </> if width == M.Width32 then "visual32.im" else "visual64.im"
  in do
    createDirectoryIfMissing True destDir
    copyFile srcFilePath destFilePath


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


runImage :: M.Image -> (Maybe String) -> (Maybe M.Script) -> IO ()
runImage image _maybeHost _maybeScript = do
    width <- M.width image
    let executableFilePath = M.visualExecutable (M.release image) M.OSX width
        imageFileName = M.path image </> if width == M.Width32 then "visual32.im" else "visual64.im"
    _ <- system $ executableFilePath ++ " " ++ imageFileName -- discard exit code
    pure ()
