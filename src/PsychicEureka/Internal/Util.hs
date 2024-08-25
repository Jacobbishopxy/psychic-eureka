{-# LANGUAGE LambdaCase #-}

-- file: Util.hs
-- author: Jacob Xie
-- date: 2024/08/19 10:06:31 Monday
-- brief:

module PsychicEureka.Internal.Util where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Data (Proxy, Typeable)
import qualified Data.List as List
import Data.Typeable (typeRep)
import GHC.Exception (throw)
import PsychicEureka.Error
import PsychicEureka.Util (Id, id2str)
import System.Directory (doesFileExist)

-- Helper function to construct a file path for an entity, given a directory, type, and ID.
getPath :: (Typeable a) => Proxy a -> FilePath -> Id -> FilePath
getPath a d i = d <> show (typeRep a) <> "." <> id2str i <> ".json"

-- Helper function to determine a file whether matches an entity
isFileMatch :: (Typeable a) => Proxy a -> FilePath -> Bool
isFileMatch a d = List.isPrefixOf (show $ typeRep a) d && List.isSuffixOf ".json" d

-- Helper function to decode a JSON file into an entity. Throws an error if the file doesn't exist or cannot be parsed.
decodeFile :: (FromJSON a) => FilePath -> IO a
decodeFile jsonFilename =
  doesFileExist jsonFilename >>= \case
    True ->
      eitherDecodeFileStrict jsonFilename
        >>= \case
          Left msg -> throw $ InternalError $ "could not parse data: " <> msg
          Right e -> return e
    _ -> throw $ EntityNotFound $ "could not find: " <> jsonFilename

-- Helper function to
decodeFileOrCreate :: (FromJSON a, ToJSON a) => FilePath -> a -> IO a
decodeFileOrCreate jsonFilename defaultValue = do
  fileExists <- doesFileExist (jsonFilename)
  if fileExists
    then decodeFile jsonFilename
    else do
      -- Write the default value to the file
      encodeFile jsonFilename defaultValue
      -- Return the default value
      return defaultValue

-- Helper function to save an entity as a JSON file. The entity is encoded to JSON and written to the specified file.
saveFile :: (Typeable a, ToJSON a) => Proxy a -> FilePath -> Id -> a -> IO a
saveFile a d i e = encodeFile (getPath a d i) e >> return e
