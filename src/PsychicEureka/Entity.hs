{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- file: Entity.hs
-- author: Jacob Xie
-- date: 2024/07/27 15:16:38 Saturday
-- brief:

module PsychicEureka.Entity where

import Control.Exception (throw)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Data (Proxy (..), TypeRep, Typeable, typeRep)
import qualified Data.List as List
import Data.Time (UTCTime)
import PsychicEureka.Error (EurekaError (..))
import PsychicEureka.Util (Id, id2str)
import System.Directory (doesFileExist, listDirectory, removeFile)

----------------------------------------------------------------------------------------------------
-- Entity
----------------------------------------------------------------------------------------------------

class NameEntity a where
  getName :: a -> String

type EntityConstraint a =
  ( ToJSON a,
    FromJSON a,
    Eq a,
    NameEntity a,
    NameEntity (EntityInput a),
    Typeable a
  )

class (EntityConstraint a) => Entity a where
  type EntityInput a

  getId :: a -> Id

  getCreatedTime :: a -> UTCTime

  getModifiedTime :: a -> UTCTime

  createFromInput :: EntityInput a -> IO a

  modifyFromInput :: EntityInput a -> a -> IO a

  ----------------------------------------------------------------------------------------------------
  -- default impl

  retrieve :: Id -> IO a
  retrieve = decodeFile . getPath tr
    where
      tr = typeRep (Proxy :: Proxy a)

  retrieveAll :: IO [a]
  retrieveAll =
    listDirectory dataDir >>= \allFiles ->
      mapM (decodeFile . (dataDir ++)) (List.filter filterFn allFiles)
    where
      tr = typeRep (Proxy :: Proxy a)
      filterFn n = List.isPrefixOf (show tr) n && List.isSuffixOf ".json" n

  save :: EntityInput a -> IO a
  save inp =
    createFromInput inp
      >>= \e -> saveFile tr (getId e) e
    where
      tr = typeRep (Proxy :: Proxy a)

  update :: Id -> EntityInput a -> IO a
  update i inp =
    retrieve i
      >>= modifyFromInput inp
      >>= saveFile tr i
    where
      tr = typeRep (Proxy :: Proxy a)

  delete :: Id -> IO a
  delete i =
    retrieve i >>= \e -> removeFile jsonFilename >> return e
    where
      tr = typeRep (Proxy :: Proxy a)
      jsonFilename = getPath tr i

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

dataDir :: FilePath
dataDir = "./data/"

getPath :: TypeRep -> Id -> String
getPath tr i = dataDir <> show tr <> "." <> id2str i <> ".json"

decodeFile :: (FromJSON a) => String -> IO a
decodeFile jsonFilename =
  doesFileExist jsonFilename >>= \case
    True ->
      eitherDecodeFileStrict jsonFilename
        >>= \case
          Left msg -> throw $ InternalError $ "could not parse data: " <> msg
          Right e -> return e
    _ -> throw $ EntityNotFound $ "could not find: " <> jsonFilename

saveFile :: (ToJSON a) => TypeRep -> Id -> a -> IO a
saveFile tr i e = encodeFile (getPath tr i) e >> return e
