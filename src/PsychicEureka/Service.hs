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

class (Cache.EntityCache a) => EntityService a where
  -- get entity name and time (testing)
  getEntityNameAndTime :: Proxy a -> Handler String
  getEntityNameAndTime p = liftIO getNowString >>= \nw -> return $ show (typeRep p) <> "\n" <> nw

  getEntityNameMap :: Cache.EntityCacheStore a -> Handler Cache.NameIdMapping
  getEntityNameMap = liftIO . Cache.getNameMap

  idExists :: Cache.EntityCacheStore a -> Id -> Handler Bool
  idExists cs i = liftIO $ Cache.isIdInCache cs i

  getEntityIdByName :: Cache.EntityCacheStore a -> String -> Handler Id
  getEntityIdByName cs n =
    liftIO (try $ Cache.getIdByName cs n) >>= handleOutput

  getEntity :: Cache.EntityCacheStore a -> Id -> Handler a
  getEntity cs i =
    liftIO (try $ Cache.retrieve cs i) >>= handleOutput

  getEntityByName :: Cache.EntityCacheStore a -> String -> Handler a
  getEntityByName cs n =
    liftIO (try $ Cache.retrieveByName cs n) >>= handleOutput

  getEntities :: Cache.EntityCacheStore a -> [Id] -> Handler [a]
  getEntities cs is = liftIO $ Cache.retrieveMany cs is

  getEntitiesByName :: Cache.EntityCacheStore a -> [String] -> Handler [a]
  getEntitiesByName cs ns = liftIO $ Cache.retrieveManyByName cs ns

  getEntities' :: Cache.EntityCacheStore a -> String -> Handler [a]
  getEntities' cs s = liftIO $ Cache.retrieveMany cs (str2ids s)

  getEntitiesByName' :: Cache.EntityCacheStore a -> String -> Handler [a]
  getEntitiesByName' cs s = liftIO $ Cache.retrieveManyByName cs (str2strs s)

  getAllEntities :: Cache.EntityCacheStore a -> Handler [a]
  getAllEntities cs =
    liftIO (try $ Cache.retrieveAll cs) >>= handleOutput

  postEntity :: Cache.EntityCacheStore a -> Entity.EntityInput a -> Handler a
  postEntity cs inp =
    liftIO (try $ Cache.save cs inp) >>= handleOutput

  putEntity :: Cache.EntityCacheStore a -> Id -> Entity.EntityInput a -> Handler a
  putEntity cs i inp =
    liftIO (try $ Cache.update cs i inp) >>= handleOutput

  putEntityByName :: Cache.EntityCacheStore a -> String -> Entity.EntityInput a -> Handler a
  putEntityByName cs n inp =
    liftIO (try $ Cache.updateByName cs n inp) >>= handleOutput

  deleteEntity :: Cache.EntityCacheStore a -> Id -> Handler a
  deleteEntity cs i =
    liftIO (try $ Cache.remove cs i) >>= handleOutput

  deleteEntityByName :: Cache.EntityCacheStore a -> String -> Handler a
  deleteEntityByName cs n =
    liftIO (try $ Cache.removeByName cs n) >>= handleOutput

----------------------------------------------------------------------------------------------------

throwAsServerError :: (MonadError ServerError m) => EurekaError -> m a
throwAsServerError ex =
  throwError $ case ex of
    EntityNotFound msg -> err404 {errBody = pack msg}
    EntityAlreadyExists msg -> err409 {errBody = pack msg}
    IdNotFound i -> err404 {errBody = pack $ id2str i}
    IdAlreadyExists i -> err409 {errBody = pack $ id2str i}
    InternalError msg -> err500 {errBody = pack msg}

handleOutput :: Either EurekaError a -> Handler a
handleOutput = \case
  Left ex -> throwAsServerError ex
  Right e -> return e
