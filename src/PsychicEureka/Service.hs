{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- file: Service.hs
-- author: Jacob Xie
-- date: 2024/07/27 19:28:46 Saturday
-- brief:

module PsychicEureka.Service
  ( EntityService (..),
  )
where

import Control.Exception (try)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy.Char8 (pack)
import Data.Data (typeRep)
import qualified PsychicEureka.Cache as Cache
import qualified PsychicEureka.Entity as Entity
import PsychicEureka.Error (EurekaError (..))
import PsychicEureka.Util (Id, getNowString, id2str, str2ids, str2strs)
import Servant
  ( Handler,
    Proxy,
    ServerError (errBody),
    err404,
    err409,
    err500,
    throwError,
  )

----------------------------------------------------------------------------------------------------
-- EntityService
----------------------------------------------------------------------------------------------------

-- | The `EntityService` typeclass defines an interface for interacting with entities in a cache.
class (Cache.EntityCache a) => EntityService a where
  -- | Get the name and current time of the entity type `a`.
  -- This function is primarily used for testing purposes.
  getEntityNameAndTime :: Proxy a -> Handler String
  getEntityNameAndTime p = liftIO getNowString >>= \nw -> return $ show (typeRep p) <> "\n" <> nw

  -- | Retrieve a mapping of entity names to their corresponding IDs.
  getEntityNameMap :: Cache.EntityCacheStore a -> Handler Cache.NameIdMapping
  getEntityNameMap = liftIO . Cache.getNameMap

  -- | Check if an entity with a given ID exists in the cache.
  idExists :: Cache.EntityCacheStore a -> Id -> Handler Bool
  idExists cs i = liftIO $ Cache.isIdInCache cs i

  -- | Retrieve an entity's ID by its name.
  getEntityIdByName :: Cache.EntityCacheStore a -> String -> Handler Id
  getEntityIdByName cs n =
    liftIO (try $ Cache.getIdByName cs n) >>= handleOutput

  -- | Retrieve an entity by its ID.
  getEntity :: Cache.EntityCacheStore a -> Id -> Handler a
  getEntity cs i =
    liftIO (try $ Cache.retrieve cs i) >>= handleOutput

  -- | Retrieve an entity by its name.
  getEntityByName :: Cache.EntityCacheStore a -> String -> Handler a
  getEntityByName cs n =
    liftIO (try $ Cache.retrieveByName cs n) >>= handleOutput

  -- | Retrieve multiple entities by their IDs.
  getEntities :: Cache.EntityCacheStore a -> [Id] -> Handler [a]
  getEntities cs is = liftIO $ Cache.retrieveMany cs is

  -- | Retrieve multiple entities by their names.
  getEntitiesByName :: Cache.EntityCacheStore a -> [String] -> Handler [a]
  getEntitiesByName cs ns = liftIO $ Cache.retrieveManyByName cs ns

  -- | Retrieve multiple entities using a comma-separated string of IDs.
  getEntities' :: Cache.EntityCacheStore a -> String -> Handler [a]
  getEntities' cs s = liftIO $ Cache.retrieveMany cs (str2ids s)

  -- | Retrieve multiple entities using a comma-separated string of names.
  getEntitiesByName' :: Cache.EntityCacheStore a -> String -> Handler [a]
  getEntitiesByName' cs s = liftIO $ Cache.retrieveManyByName cs (str2strs s)

  -- | Retrieve all entities from the cache.
  getAllEntities :: Cache.EntityCacheStore a -> Handler [a]
  getAllEntities cs =
    liftIO (try $ Cache.retrieveAll cs) >>= handleOutput

  -- | Save a new entity to the cache using the provided input data.
  postEntity :: Cache.EntityCacheStore a -> Entity.EntityInput a -> Handler a
  postEntity cs inp =
    liftIO (try $ Cache.save cs inp) >>= handleOutput

  -- | Update an existing entity in the cache by its ID using the provided input data.
  putEntity :: Cache.EntityCacheStore a -> Id -> Entity.EntityInput a -> Handler a
  putEntity cs i inp =
    liftIO (try $ Cache.update cs i inp) >>= handleOutput

  -- | Update an existing entity in the cache by its name using the provided input data.
  putEntityByName :: Cache.EntityCacheStore a -> String -> Entity.EntityInput a -> Handler a
  putEntityByName cs n inp =
    liftIO (try $ Cache.updateByName cs n inp) >>= handleOutput

  -- | Delete an entity from the cache by its ID.
  deleteEntity :: Cache.EntityCacheStore a -> Id -> Handler a
  deleteEntity cs i =
    liftIO (try $ Cache.remove cs i) >>= handleOutput

  -- | Delete an entity from the cache by its name.
  deleteEntityByName :: Cache.EntityCacheStore a -> String -> Handler a
  deleteEntityByName cs n =
    liftIO (try $ Cache.removeByName cs n) >>= handleOutput

----------------------------------------------------------------------------------------------------

-- | Converts an `EurekaError` into a `ServerError` and throws it.
-- This function handles various application-specific errors by mapping them to HTTP status codes.
throwAsServerError :: (MonadError ServerError m) => EurekaError -> m a
throwAsServerError ex =
  throwError $ case ex of
    EntityNotFound msg -> err404 {errBody = pack msg}
    EntityAlreadyExists msg -> err409 {errBody = pack msg}
    IdNotFound i -> err404 {errBody = pack $ id2str i}
    IdAlreadyExists i -> err409 {errBody = pack $ id2str i}
    InternalError msg -> err500 {errBody = pack msg}

-- | Handles the output of `Either EurekaError a` by either throwing a server error or returning the result.
handleOutput :: Either EurekaError a -> Handler a
handleOutput = \case
  Left ex -> throwAsServerError ex
  Right e -> return e
