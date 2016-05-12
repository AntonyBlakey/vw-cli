{-# LANGUAGE FlexibleInstances #-}
module Model
    (
      Nameable, Pathish
    , Root(..), Release, Image, Backup, Script
    , Width(..), Platform(..)
    , releases, images, backups, scripts
    , path, root, relativePath, name, tags, identifiers
    , devCycle, release, image, width, visualExecutable, timestamp
    , rewritePathForVM, addTag, removeTag
    ) where

import           Control.Monad
import           Control.Monad.Extra (ifM)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Typeable
import           System.Directory
import           System.FilePath


data Width = Width32 | Width64 deriving (Eq)
instance Show Width where
  show x = if x == Width32 then "32" else "64"

data Platform = OSX | Windows | Linux deriving (Eq, Show)

newtype Root    = Root    { rootPath :: FilePath } deriving (Eq, Show, Typeable)
newtype Release = Release { releasePath :: FilePath } deriving (Eq, Show, Typeable)
newtype Image   = Image   { imagePath :: FilePath } deriving (Eq, Show, Typeable)
newtype Backup  = Backup  { backupPath :: FilePath } deriving (Eq, Show, Typeable)
newtype Script  = Script  { scriptPath :: FilePath } deriving (Eq, Show, Typeable)


class Pathish a where
  path :: a -> FilePath
  root :: a -> Root
  relativePath :: a -> FilePath
  relativePath this = makeRelative (path $ root this) (path this)


class (Pathish a, Typeable a) => Nameable a where
  name :: a -> String
  name = takeFileName . path
  tags :: a -> IO [String]
  tags _ = pure []
  identifiers :: a -> IO [String]
  identifiers a = (name a :) <$> tags a


class ImageContainer a where
  images :: a -> IO [Image]


-- Platform

rewritePathForVM :: FilePath -> Platform -> FilePath
rewritePathForVM = undefined


-- Root

instance Pathish Root where
  path = rootPath
  root = id

releases :: Root -> IO [Release]
releases = fmap (map Release) . directories . (</> "releases") . path

instance ImageContainer Root where
  images = fmap concat . mapM images <=< releases

scripts :: Root -> IO [Script]
scripts = fmap (map Script . filter ((== ".st") . takeExtension)) . files . (</> "scripts") . path


-- Release

instance Pathish Release where
  path = releasePath
  root = Root . takeDirectory . takeDirectory . path

instance Nameable Release where
  tags = fmap (map (drop 1 . takeExtensions) . filter ((== "tag") . takeFileName . dropExtensions)) . files . path

instance ImageContainer Release where
  images = fmap (map Image) . directories . (</> "images") . path

devCycle :: Release -> String
devCycle = takeWhile (/= '-') . name

visualExecutable :: Release -> Platform -> Width -> FilePath
visualExecutable r OSX Width32 = foldl (</>) (path r) ["release", "bin", "macx", "visual.app", "Contents", "MacOS", "visual"]
visualExecutable r OSX Width64 = foldl (</>) (path r) ["release", "preview", "bin", "macx64", "visual.app", "Contents", "MacOS", "visual"]
visualExecutable r Windows Width32 = foldl (</>) (path r) ["release", "bin", "win", "visual.exe"]
visualExecutable r Windows Width64 = foldl (</>) (path r) ["release", "bin", "win64", "visual.exe"]
visualExecutable r Linux Width32 = foldl (</>) (path r) ["release", "bin", "linux86", "visual"]
visualExecutable r Linux Width64 = foldl (</>) (path r) ["release", "bin", "linuxx86_64", "visual"]

tagPath :: Release -> String -> FilePath
tagPath rls tagName = path rls </> "tag" <.> tagName

addTag :: Release -> String -> IO ()
addTag rls tagName = writeFile (tagPath rls tagName) ""

removeTag :: Release -> String -> IO ()
removeTag rls tagName = do
  let file = tagPath rls tagName
  fileExists <- doesFileExist file
  when fileExists $ removeFile file


-- Image

instance Pathish Image where
  path = imagePath
  root = root . release

instance Nameable Image

release :: Image -> Release
release = Release . takeDirectory . takeDirectory . path

width :: Image -> IO Width
width i = ifM (doesFileExist $ path i </> "visual32.im") (pure Width32) (pure Width64)

backups :: Image -> IO [Backup]
backups = fmap (map Backup) . directories . (</> "backups") . path


-- Backup

instance Pathish Backup where
  path = backupPath
  root = root . image

instance Nameable Backup where
  name = drop 1 . takeExtensions . path
  tags backup = pure [show $ timestamp backup]

image :: Backup -> Image
image = Image . takeDirectory . path

timestamp :: Backup -> UTCTime
timestamp backup = parseTimeOrError False defaultTimeLocale "%Y-%m-%d-%H-%M-%S" (takeBaseName (path backup))


-- Script

instance Pathish Script where
  path = scriptPath
  root = Root . takeDirectory . takeDirectory . path

instance Nameable Script where
  name = takeFileName . path


-- Utilities

safeGetDirectoryContents :: FilePath -> IO [FilePath]
safeGetDirectoryContents p = ifM (doesDirectoryExist p) (getDirectoryContents p) (pure [])

visibleAbsoluteDirectoryContents :: FilePath -> IO [FilePath]
visibleAbsoluteDirectoryContents p = mapM makeAbsolute =<< map (p </>) . filter ((/= '.') . head) <$> safeGetDirectoryContents p

directories :: FilePath -> IO [FilePath]
directories = filterM doesDirectoryExist <=< visibleAbsoluteDirectoryContents

files :: FilePath -> IO [FilePath]
files = filterM doesFileExist <=< visibleAbsoluteDirectoryContents
