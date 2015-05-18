-- | This module contains functions to produce PNG images of raw documents.
module Alexandria.Render where

import Control.Applicative ((<$>))
import Control.Monad (liftM, when)
import Data.List (isPrefixOf,stripPrefix)
import Data.String.Utils (split, strip)
import System.Directory (getModificationTime)
import System.FilePath
import System.Posix.Files (fileExist)
import System.Process (callProcess, readProcess)

import Alexandria.Config
import Alexandria.Types

-- | Convert a PDF file to a PNG image.
pdfToPng :: Configuration a => a -> FilePath -> FilePath -> IO ()
pdfToPng conf pdf output = callProcess "convert" ["-quality", show $ quality conf,
                                                  "-density", show $ dpi conf,
                                                  pdf, output]

-- | Compile a complete LaTeX file and put the resulting PDF into the temporary
-- directory.
latexToPdf :: Configuration a => a -> FilePath -> FilePath -> IO ()
latexToPdf conf input tempDirectory = callProcess "rubber" ["--module", "xelatex",
                                                            "--force", "--into", tempDirectory,
                                                            input]
generateIndex :: Configuration a => a -> IO String
generateIndex conf = readProcess "index++" params ""
  where params = ["-c", swisheConfig conf, knowledgeDirectory conf]

searchSwishe :: Configuration a => a -> [String] -> IO String
searchSwishe conf args = readProcess "search++" params ""
  where params = ["-c", swisheConfig conf] ++ args



-- Analyse a document

-- | Find out what kind of headers and footers to use.  The document type is
-- given by the first tag.
documentType :: Configuration a => a -> String -> Maybe DocumentType
documentType conf contents = head <$> getTags conf contents

-- TODO replace this function with its ParseTex equivalent
-- | Extract the tags line from a document.
getTags :: Configuration a => a -> String -> Maybe [Tag]
getTags conf contents = do
  tagsString <- stripPrefix "%" $ last $ lines contents
  return $ map strip $ split "," tagsString

-- | Read a document given its id.
readDoc :: Configuration a => a -> Id -> IO String
readDoc conf id = readFile (knowledgeDirectory conf </> id <.> "tex")

-- Produce a PNG from some LaTeX snippets

-- | Render a LaTeX document to a PNG file given the document type and the id.
render :: Configuration a => a -> String -> Id -> IO ()
render conf typ id = do
  -- Read the correct type of static headers and footers
  headers <- mapM (readFile . (templateDirectory conf </>)) ["header.tex", typ ++ "_header.tex"]
  footers <- mapM (readFile . (templateDirectory conf </>)) [typ ++ "_footer.tex", "footer.tex"]
  -- Read the content in question
  main <- readDoc conf id
  -- Combine it with the headers
  let content = concat headers ++ main ++ concat footers
  -- And write the generated file to disk.
  let tempFile = tempDirectory conf </> id <.> "tex"
  writeFile tempFile content

  -- Compile the LaTeX code to first produce a PDF file.
  latexToPdf conf tempFile (tempDirectory conf)

  let pdfFile = replaceExtension tempFile "pdf"
  let pngFile = cacheDirectory conf </> replaceExtension (takeFileName tempFile) "png"
  -- and convert the PDF into a PNG image.
  pdfToPng conf pdfFile pngFile

-- | Check whether a PNG of the document with the given id is in the cache and,
-- if so, whether it is up to date.
-- TODO make the code nicer (Control.Monad?)
needToRefresh :: Configuration a => a -> Id -> IO Bool
needToRefresh conf id = do
  content <- readDoc conf id
  let typ = case documentType conf content of
              Nothing -> fail ("no document type found in document " ++ id)
              (Just t) -> t
  dates <- mapM getModificationTime
                        [knowledgeDirectory conf </> id <.> "tex",
                         templateDirectory conf </> "header.tex",
                         templateDirectory conf </> "footer.tex",
                         templateDirectory conf </> typ ++ "_header.tex",
                         templateDirectory conf </> typ ++ "_footer.tex"]
  let outputFile = cacheDirectory conf </> id <.> "png"
  fileExists <- fileExist outputFile
  if fileExists
  then do
    let sourceDate = maximum dates
    targetDate <- getModificationTime outputFile
    return (targetDate <= sourceDate)
  else return True

-- | Figure out the document type and render the document as a document of that
-- type.  The document is given by its id.
renderAny :: Configuration a => a -> Id -> IO ()
renderAny conf id = do
  update <- needToRefresh conf id
  when update $ do
    file <- readDoc conf id
    case documentType conf file of
         Nothing -> fail ("could not find document type of document " ++ id)
         (Just typ) -> render conf typ id

-- | Given the results of a swish-e query as a list of lines, produce a list of
-- document ids and render the documents with these ids.
renderResults :: Configuration a => a -> [String] -> IO [Id]
renderResults conf []                    = fail "No output read"
renderResults conf ("err: no results":_) = fail "No result found"
renderResults conf matches               = mapM_ (renderAny conf) ids >> return ids
  where ids = map (takeBaseName . head . words) matches



-- Find and return documents (rendering them if necessary).

-- | Run a full text search to obtain an ordered list of documents matching the
-- query.
findDocs :: Configuration a => a -> [String] -> IO [Id]
findDocs conf args    = processOutput $ searchSwishe conf args
  where stripComments = dropWhile (isPrefixOf "#")
        takeResults   = map (takeBaseName . (\line -> words line !! 1)) . takeWhile (not . (== "."))
        processOutput = liftM (takeResults . stripComments . lines)
