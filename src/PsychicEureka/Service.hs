{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- file: Service.hs
-- author: Jacob Xie
-- date: 2024/07/27 19:28:46 Saturday
-- brief:

module PsychicEureka.Service where

import Control.Exception (try)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy.Char8 (pack)
import Data.Data (typeRep)
import qualified PsychicEureka.Cache as Cache
import qualified PsychicEureka.Entity as Entity
import PsychicEureka.Error (EurekaError (..))
import PsychicEureka.Util (Id, getNowString)
import Servant

----------------------------------------------------------------------------------------------------
-- EntityService
----------------------------------------------------------------------------------------------------

class (Cache.EntityCache a) => EntityService a where
  -- get entity name and time (testing)
  getEntityNameAndTime :: Proxy a -> Handler String
  getEntityNameAndTime p = liftIO getNowString >>= \nw -> return $ show (typeRep p) <> "\n" <> nw

  getEntityNameMap :: Cache.EntityCacheStore a -> Handler Cache.NameIdMapping
  getEntityNameMap = liftIO . Cache.getNameMap

  getEntityIdByName :: Cache.EntityCacheStore a -> String -> Handler Id
  getEntityIdByName ref n =
    liftIO (try $ Cache.getIdByName ref n) >>= handleOutput

  getEntity :: Cache.EntityCacheStore a -> Id -> Handler a
  getEntity ref i =
    liftIO (try $ Cache.retrieve ref i) >>= handleOutput

  getEntityByName :: Cache.EntityCacheStore a -> String -> Handler a
  getEntityByName ref n =
    liftIO (try $ Cache.retrieveByName ref n) >>= handleOutput

  postEntity :: Cache.EntityCacheStore a -> Entity.EntityInput a -> Handler a
  postEntity ref inp =
    liftIO (try $ Cache.save ref inp) >>= handleOutput

  putEntity :: Cache.EntityCacheStore a -> Id -> Entity.EntityInput a -> Handler a
  putEntity ref i inp =
    liftIO (try $ Cache.update ref i inp) >>= handleOutput

  putEntityByName :: Cache.EntityCacheStore a -> String -> Entity.EntityInput a -> Handler a
  putEntityByName ref n inp =
    liftIO (try $ Cache.updateByName ref n inp) >>= handleOutput

  deleteEntity :: Cache.EntityCacheStore a -> Id -> Handler a
  deleteEntity ref i =
    liftIO (try $ Cache.remove ref i) >>= handleOutput

  deleteEntityByName :: Cache.EntityCacheStore a -> String -> Handler a
  deleteEntityByName ref n =
    liftIO (try $ Cache.removeByName ref n) >>= handleOutput

----------------------------------------------------------------------------------------------------

throwAsServerError :: (MonadError ServerError m) => EurekaError -> m a
throwAsServerError ex =
  throwError $ case ex of
    EntityNotFound msg -> err404 {errBody = pack msg}
    EntityAlreadyExists msg -> err409 {errBody = pack msg}
    InternalError msg -> err500 {errBody = pack msg}

handleOutput :: Either EurekaError a -> Handler a
handleOutput = \case
  Left ex -> throwAsServerError ex
  Right e -> return e
