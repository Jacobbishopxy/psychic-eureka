-- file: CleanTestData.hs
-- author: Jacob Xie
-- date: 2024/08/28 09:14:08 Wednesday
-- brief:

module Main where

import Control.Monad (forM_)
import System.Directory
import System.FilePath
import Text.Regex.Posix

cleanFilesByRegex :: FilePath -> String -> IO ()
cleanFilesByRegex dir regex = do
  files <- listDirectory dir

  let filesToDelete = filter (=~ regex) files

  forM_ filesToDelete $ \file -> do
    let fp = dir </> file
    removeFile fp
    putStrLn $ "Deleted: " <> fp

main :: IO ()
main = do
  let directory = "./data"
  let regex = "^.*\\.json$"

  cleanFilesByRegex directory regex
