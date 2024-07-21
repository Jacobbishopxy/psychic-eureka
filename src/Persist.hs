{-# LANGUAGE LambdaCase #-}
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
import Data.Data (TypeRep)
import qualified Data.List as List
import Data.Typeable (Typeable, typeRep)
import Servant (Proxy)
import System.Directory
  ( doesFileExist,
    listDirectory,
    removeFile,
  )
import Util (Id)

----------------------------------------------------------------------------------------------------
-- Persist
----------------------------------------------------------------------------------------------------

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
  retrieveAll maxRecords =
    listDirectory dataDir >>= \allFiles ->
      let tr = typeRep ([] :: [a])
          filteredFiles = List.filter (filterFn tr) allFiles
          files = case maxRecords of
            Nothing -> filteredFiles
            Just n -> take n filteredFiles
       in mapM (decodeFile . (dataDir ++)) files
    where
      filterFn tr' fn = List.isPrefixOf (show tr') fn && List.isSuffixOf ".json" fn

----------------------------------------------------------------------------------------------------

dataDir :: FilePath
dataDir = "./data/"

getPath :: TypeRep -> Id -> String
getPath tr uid = dataDir <> show tr <> "." <> show uid <> ".json"

decodeFile :: (FromJSON a) => String -> IO a
decodeFile jsonFileName =
  doesFileExist jsonFileName >>= \case
    True ->
      eitherDecodeFileStrict jsonFileName
        >>= \case
          Left msg -> throw $ InternalError $ "could not parse data: " <> msg
          Right e -> return e
    _ -> throw $ EntityNotFound $ "could not find: " <> jsonFileName
