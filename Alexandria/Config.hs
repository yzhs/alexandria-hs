module Alexandria.Config where

import System.FilePath ((</>))

-- | General configuration of alexandria
class Configuration a where
  -- | Project root directory
  baseDirectory :: a -> FilePath

  -- | Directory for files generated when compiling the TeX document.
  tempDirectory :: a -> FilePath
  tempDirectory conf = baseDirectory conf </> "tmp"

  -- | TeX header and footer files live in this directory.
  templateDirectory :: a -> FilePath
  templateDirectory conf = baseDirectory conf </> "templates"

  -- | The PNG files of the various documents are stored here.
  cacheDirectory :: a -> FilePath
  cacheDirectory conf = baseDirectory conf </> "cache"

  -- | The raw documents are stored in this directory.
  knowledgeDirectory :: a -> FilePath
  knowledgeDirectory conf = baseDirectory conf </> "library"

  -- | Path to swish-e configuration file
  swisheConfig :: a -> FilePath
  swisheConfig conf = baseDirectory conf </> "swish++.conf"

  -- | Path to swish-e index file
  swisheIndex :: a -> FilePath
  swisheIndex conf = baseDirectory conf </> "index.swish-e"

  -- | The number of dots per inch for the generated PNG files
  dpi :: a -> Int
  dpi _ = 137

  -- | Quality setting for the PNG encoder.  This parameter is assumed to be
  -- at least 0 and at most 100.
  quality :: a -> Int
  quality _ = 90

data DefaultConfig = DefaultConfig

instance Configuration DefaultConfig where
  baseDirectory _ = "/home/joghurt/.alexandria"

defaultConfig = DefaultConfig
