-- | This module contains a bunch of functions for collecting statistics
-- about the library and to generate a report containing these.
module Alexandria.Stats where

import Control.Monad (liftM)
import Data.List (isPrefixOf)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Posix (fileSize, getFileStatus)

import Alexandria.Config

scrolls :: Configuration a => a -> IO [FilePath]
scrolls conf = liftM (filter (not . isPrefixOf ".")) $ getDirectoryContents $ knowledgeDirectory conf

countScrolls :: Configuration a => a -> IO Int
countScrolls conf = liftM length $ scrolls conf

countBytes :: Configuration a => a -> IO Int
countBytes conf = do
  paths <- liftM (map (knowledgeDirectory conf </>)) $ scrolls conf
  status <- mapM getFileStatus paths
  return $ sum $ map (fromIntegral . fileSize) status
