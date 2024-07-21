{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- file: Persist.hs
-- author: Jacob Xie
-- date: 2024/07/15 21:53:16 Monday
-- brief:

module Persist where

import Control.Exception (Exception, throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
  ( FromJSON,
    ToJSON,
    eitherDecodeFileStrict,
    encodeFile,
  )
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Swagger
import Data.Text (pack)
import Data.Typeable (TypeRep, Typeable, typeRep)
import Data.UUID (UUID, fromString, fromText)
import GHC.Generics
import Servant
import System.Directory
  ( doesFileExist,
    listDirectory,
    removeFile,
  )

-- newtype id
newtype Id = Id UUID
  deriving (Show, Eq, Ord, Read, Generic, Typeable, ToJSON, FromJSON, ToParamSchema, ToSchema)

instance FromHttpApiData Id where
  parseUrlPiece txt =
    case fromText txt of
      Just uuid -> Right (Id uuid)
      Nothing -> Left $ pack "Failed to parse Id"

data PersistErr
  = EntityNotFound String
  | EntityAlreadyExists String
  | InternalError String
  deriving (Show)

instance Exception PersistErr

class (ToJSON a, FromJSON a, Typeable a) => Entity a where
  -- unique Id of the entity
  getId :: a -> Id

  -- update createdAt field
  mkCreatedAt :: a -> IO a

  -- update modifiedAt field
  mkModifiedAt :: a -> IO a

  -- persist a new entity
  post :: a -> IO a
  post entity = do
    let jsonFileName = getPath (typeRep ([] :: [a])) (getId entity)
    fileExists <- doesFileExist jsonFileName
    if fileExists
      then throw $ EntityAlreadyExists $ "entity record already exists: " <> jsonFileName
      else do
        e <- liftIO $ mkCreatedAt entity
        encodeFile jsonFileName e
        return e

  -- update an entity
  put :: Id -> a -> IO a
  put uid entity = do
    let jsonFileName = getPath (typeRep ([] :: [a])) uid
    fileExists <- doesFileExist jsonFileName
    if fileExists
      then do
        e <- liftIO $ mkModifiedAt entity
        encodeFile jsonFileName e
        return e
      else throw $ EntityNotFound $ "could not update as entity was not found: " <> jsonFileName

  -- delete an entity
  delete :: Proxy a -> Id -> IO a
  delete proxy uid = do
    let jsonFileName = getPath (typeRep proxy) uid
    fileExists <- doesFileExist jsonFileName
    if fileExists
      then do
        e <- liftIO $ retrieve uid
        removeFile jsonFileName
        return e
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
dataDir = "./data/"

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

mockId :: Id
mockId = Id $ fromJust $ fromString "123e4567-e89b-12d3-a456-426614174000"
