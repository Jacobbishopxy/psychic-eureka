{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- file: Entity.hs
-- author: Jacob Xie
-- date: 2024/07/27 15:16:38 Saturday
-- brief:

module PsychicEureka.Entity
  ( NameEntity (..),
    Entity (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Proxy (..), Typeable)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified PsychicEureka.Internal.Util as Util
import PsychicEureka.Util (Id)
import System.Directory (listDirectory, removeFile)

----------------------------------------------------------------------------------------------------
-- Entity
----------------------------------------------------------------------------------------------------

class NameEntity a where
  getName :: a -> String

type EntityConstraint a =
  ( Generic a,
    ToJSON a,
    FromJSON a,
    Eq a,
    NameEntity a,
    NameEntity (EntityInput a),
    Typeable a
  )

class (EntityConstraint a) => Entity a where
  -- | Associated type representing the input required to create or modify an entity.
  type EntityInput a

  getId :: a -> Id

  getCreatedTime :: a -> UTCTime

  getModifiedTime :: a -> UTCTime

  createFromInput :: EntityInput a -> IO a

  modifyFromInput :: EntityInput a -> a -> IO a

  ----------------------------------------------------------------------------------------------------
  -- default impl

  -- | Method to get the directory where entities of this type are persisted. Uses `Proxy` to infer type `a`.
  persistDir :: Proxy a -> FilePath
  persistDir _ = "./data/"

  -- | Method to get the full path to the file where an entity is stored, based on its ID.
  getPath :: Proxy a -> Id -> FilePath
  getPath _ i = Util.getPath a (persistDir a) i
    where
      a = Proxy @a

  -- | Method to retrieve an entity by its ID. Reads from the corresponding file and decodes it from JSON.
  retrieve :: Id -> IO a
  retrieve = Util.decodeFile . getPath a
    where
      a = Proxy @a

  -- | Method to retrieve all entities of this type from the persistence directory.
  retrieveAll :: IO [a]
  retrieveAll =
    listDirectory (persistDir a) >>= \allFiles ->
      mapM (Util.decodeFile . (persistDir a ++)) (filter (Util.isFileMatch a) allFiles)
    where
      a = Proxy @a

  -- | Method to save a new entity based on the input data. The entity is saved to a file.
  save :: EntityInput a -> IO a
  save inp =
    createFromInput inp
      >>= \e -> Util.saveFile a (persistDir a) (getId e) e
    where
      a = Proxy @a

  -- | Method to update an existing entity by its ID using new input data. The updated entity is saved to a file.
  update :: Id -> EntityInput a -> IO a
  update i inp =
    retrieve i
      >>= modifyFromInput inp
      >>= Util.saveFile a (persistDir a) i
    where
      a = Proxy @a

  -- | Method to delete an entity by its ID. The corresponding file is removed.
  delete :: Id -> IO a
  delete i =
    retrieve i >>= \e -> removeFile jsonFilename >> return e
    where
      a = Proxy @a
      jsonFilename = getPath a i
