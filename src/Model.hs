{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model
    (
      Nameable
    , Root(..), Release, Image, Backup, Script
    , Width(..), Platform(..)
    , releases, images, backups, scripts
    , path, name, identifiers, tags, root, devCycle, release, image, width, visualExecutable, timestamp
    , rewritePathForVM, addTag, removeTag
    ) where

import           Control.Monad
import           Control.Monad.Extra (ifM)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Typeable
import           System.Directory
import           System.FilePath


data Width = Width32 | Width64 deriving (Eq, Show)
data Platform = OSX | Windows | Linux deriving (Eq, Show)

newtype Root = Root FilePath deriving (Eq, Show, Typeable, Pathish)
newtype Release = Release FilePath deriving (Eq, Show, Typeable, Pathish)
newtype Image =  Image FilePath deriving (Eq, Show, Typeable, Pathish)
newtype Backup = Backup FilePath deriving (Eq, Show, Typeable, Pathish)
newtype Script = Script FilePath deriving (Eq, Show, Typeable, Pathish)

class Pathish a where
  path :: a -> FilePath

instance Pathish FilePath where
  path = id

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

releases :: Root -> IO [Release]
releases = fmap (map Release) . directories . (</> "releases") . path

instance ImageContainer Root where
  images = fmap concat . mapM images <=< releases

scripts :: Root -> IO [Script]
scripts = fmap (map Script . filter ((== ".st") . takeExtension)) . files . (</> "scripts") . path


-- Release

instance Nameable Release where
  tags = fmap (map (drop 1 . takeExtensions) . filter ((== "tag") . takeFileName . dropExtensions)) . files . path

instance ImageContainer Release where
  images = fmap (map Image) . directories . (</> "images") . path

devCycle :: Release -> String
devCycle = name

root :: Release -> Root
root = Root . takeDirectory . path

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

instance Nameable Image

release :: Image -> Release
release = Release . takeDirectory . takeDirectory . path

width :: Image -> IO Width
width i = ifM (doesFileExist $ path i </> "visual32.im") (pure Width32) (pure Width64)

backups :: Image -> IO [Backup]
backups = fmap (map Backup) . directories . (</> "backups") . path


-- Backup

instance Nameable Backup where
  name = drop 1 . takeExtensions . path
  tags backup = pure [show $ timestamp backup]

image :: Backup -> Image
image = Image . takeDirectory . path

timestamp :: Backup -> UTCTime
timestamp backup = parseTimeOrError False defaultTimeLocale "%Y-%m-%d-%H-%M-%S" (takeBaseName (path backup))


-- Script

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
