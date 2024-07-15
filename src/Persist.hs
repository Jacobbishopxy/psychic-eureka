{-# LANGUAGE ScopedTypeVariables #-}

-- file: Persist.hs
-- author: Jacob Xie
-- date: 2024/07/15 21:53:16 Monday
-- brief:

module Persist where

import Control.Exception (Exception, throw)
import Data.Aeson
  ( FromJSON,
    ToJSON,
    eitherDecodeFileStrict,
    encodeFile,
  )
import qualified Data.List as List
import Data.Typeable (Proxy, TypeRep, Typeable, typeRep)
import Data.UUID (UUID)
import System.Directory
  ( doesFileExist,
    listDirectory,
    removeFile,
  )

type Id = UUID

data PersistErr
  = EntityNotFound String
  | EntityAlreadyExists String
  | InternalError String
  deriving (Show)

instance Exception PersistErr

class (ToJSON a, FromJSON a, Typeable a) => Entity a where
  -- unique Id of the entity
  getId :: a -> Id

  -- persist a new entity
  post :: a -> IO ()
  post entity = do
    let jsonFileName = getPath (typeRep ([] :: [a])) (getId entity)
    fileExists <- doesFileExist jsonFileName
    if fileExists
      then throw $ EntityAlreadyExists $ "entity record already exists: " <> jsonFileName
      else encodeFile jsonFileName entity

  -- update an entity
  put :: Id -> a -> IO ()
  put uid entity = do
    let jsonFileName = getPath (typeRep ([] :: [a])) uid
    fileExists <- doesFileExist jsonFileName
    if fileExists
      then encodeFile jsonFileName entity
      else throw $ EntityNotFound $ "could not update as entity was not found: " <> jsonFileName

  -- delete an entity
  delete :: Proxy a -> Id -> IO ()
  delete proxy uid = do
    let jsonFileName = getPath (typeRep proxy) uid
    fileExists <- doesFileExist jsonFileName
    if fileExists
      then removeFile jsonFileName
      else throw $ EntityNotFound $ "could not delete as entity was not found: " <> jsonFileName

  -- load persistent entity
  retrieve :: Id -> IO a
  retrieve uid = decodeFile $ getPath (typeRep ([] :: [a])) uid

  -- load all persistent entities
  retrieveAll :: Maybe Int -> IO [a]
  retrieveAll maxRecords = do
    let tr = typeRep ([] :: [a])
    allFiles <- listDirectory dataDir
    let filteredFiles = List.filter (\fname -> List.isPrefixOf (show tr) fname && List.isSuffixOf ".json" fname) allFiles
        files = case maxRecords of
          Nothing -> filteredFiles
          Just n -> take n filteredFiles
    mapM (\fname -> decodeFile (dataDir ++ fname)) files

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

dataDir :: FilePath
dataDir = ".data/"

getPath :: TypeRep -> Id -> String
getPath tr uid = dataDir <> show tr <> "." <> show uid <> ".json"

decodeFile :: (FromJSON a) => String -> IO a
decodeFile jsonFileName = do
  fileExists <- doesFileExist jsonFileName
  if fileExists
    then
      eitherDecodeFileStrict jsonFileName
        >>= \eitherEntity -> case eitherEntity of
          Left msg -> throw $ InternalError $ "could not parse data: " <> msg
          Right e -> return e
    else
      throw $ EntityNotFound $ "could not find: " <> jsonFileName
