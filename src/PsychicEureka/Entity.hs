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

import Control.Exception (throw)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Data (Proxy (..), Typeable, typeRep)
import qualified Data.List as List
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import PsychicEureka.Error (EurekaError (..))
import PsychicEureka.Util (Id, id2str)
import System.Directory (doesFileExist, listDirectory, removeFile)

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
  type EntityInput a

  getId :: a -> Id

  getCreatedTime :: a -> UTCTime

  getModifiedTime :: a -> UTCTime

  createFromInput :: EntityInput a -> IO a

  modifyFromInput :: EntityInput a -> a -> IO a

  ----------------------------------------------------------------------------------------------------
  -- default impl

  persistDir :: Proxy a -> FilePath
  persistDir _ = "./data"

  getPath :: Proxy a -> Id -> FilePath
  getPath _ i = (persistDir a) <> show (typeRep a) <> "." <> id2str i <> ".json"
    where
      a = Proxy @a

  retrieve :: Id -> IO a
  retrieve = decodeFile . getPath a
    where
      a = Proxy @a

  retrieveAll :: IO [a]
  retrieveAll =
    listDirectory (persistDir a) >>= \allFiles ->
      mapM (decodeFile . (persistDir a ++)) (List.filter filterFn allFiles)
    where
      a = Proxy @a
      filterFn n = List.isPrefixOf (show $ typeRep a) n && List.isSuffixOf ".json" n

  save :: EntityInput a -> IO a
  save inp =
    createFromInput inp
      >>= \e -> saveFile a (persistDir a) (getId e) e
    where
      a = Proxy @a

  update :: Id -> EntityInput a -> IO a
  update i inp =
    retrieve i
      >>= modifyFromInput inp
      >>= saveFile a (persistDir a) i
    where
      a = Proxy @a

  delete :: Id -> IO a
  delete i =
    retrieve i >>= \e -> removeFile jsonFilename >> return e
    where
      a = Proxy @a
      jsonFilename = getPath a i

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

getPath' :: (Typeable a) => Proxy a -> FilePath -> Id -> FilePath
getPath' a d i = d <> show (typeRep a) <> "." <> id2str i <> ".json"

decodeFile :: (FromJSON a) => FilePath -> IO a
decodeFile jsonFilename =
  doesFileExist jsonFilename >>= \case
    True ->
      eitherDecodeFileStrict jsonFilename
        >>= \case
          Left msg -> throw $ InternalError $ "could not parse data: " <> msg
          Right e -> return e
    _ -> throw $ EntityNotFound $ "could not find: " <> jsonFilename

saveFile :: (Typeable a, ToJSON a) => Proxy a -> FilePath -> Id -> a -> IO a
saveFile a d i e = encodeFile (getPath' a d i) e >> return e
