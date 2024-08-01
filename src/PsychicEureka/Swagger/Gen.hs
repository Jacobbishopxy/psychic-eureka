{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- file: Schema.hs
-- author: Jacob Xie
-- date: 2024/07/27 16:22:36 Saturday
-- brief:

module PsychicEureka.Swagger.Gen
  ( Desc,
    EntityAPI,
    SwaggerAPI,
    EntitySwaggerAPI,
    DocInfo (..),
    generateSwagger,
    entityServer,
    swaggerServer,
    printApiInfo,
  )
where

import Control.Lens ((&), (.~), (?~))
import Data.Swagger
import Data.Text (pack)
import GHC.TypeLits (KnownSymbol)
import qualified PsychicEureka.Cache as Cache
import qualified PsychicEureka.Service as Service
import PsychicEureka.Swagger.TypeF
import Servant (Proxy, Server, type (:<|>) ((:<|>)))
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.UI (swaggerSchemaUIServer)

----------------------------------------------------------------------------------------------------

-- | 'DocInfo' represents the documentation metadata for the Swagger documentation.
data DocInfo = DocInfo
  { docTitle :: String,
    docVersion :: String,
    docDescription :: String,
    docTag :: String,
    docTagDescription :: Maybe String
  }

-- | 'generateSwagger' generates the Swagger documentation for a given 'EntityAPI'.
-- It uses the provided 'DocInfo' to fill in the metadata.
generateSwagger ::
  (HasSwagger (EntityAPI entity a), KnownSymbol entity) =>
  -- | Proxy for the 'EntityAPI' type.
  Proxy (EntityAPI entity a) ->
  -- | Documentation metadata.
  DocInfo ->
  -- | Generated Swagger documentation.
  Swagger
generateSwagger apiProxy (DocInfo t v d tg tgd) =
  toSwagger apiProxy
    & info . title .~ (pack t)
    & info . version .~ (pack v)
    & info . description ?~ (pack d)
    & info . license ?~ ("BSD 3.0" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")
    & applyTags [Tag (pack tg) (pack <$> tgd) Nothing]

----------------------------------------------------------------------------------------------------
-- entityServer & swaggerServer
--
-- the minimum use case of the `EntityAPI`.
-- In the real world application, we need complex APIs, so multiple `EntityAPI`s and custom APIs
-- should be seen.
----------------------------------------------------------------------------------------------------

-- | 'entityServer' creates the server for the 'EntityAPI' for a given entity.
entityServer ::
  forall name entity.
  (Service.EntityService entity, KnownSymbol name) =>
  -- | Proxy for the entity name prefix.
  Proxy name ->
  -- | Proxy for the entity type.
  Proxy entity ->
  -- | Cache store for the entity.
  Cache.EntityCacheStore entity ->
  -- | Server for the 'EntityAPI'.
  Server (EntityAPI name entity)
entityServer _ p ecs =
  Service.getEntityNameAndTime p
    :<|> Service.getEntityNameMap ecs
    :<|> Service.getEntityIdByName ecs
    :<|> Service.getEntity ecs
    :<|> Service.getEntityByName ecs
    :<|> Service.getAllEntities ecs
    :<|> Service.postEntity ecs
    :<|> Service.putEntity ecs
    :<|> Service.putEntityByName ecs
    :<|> Service.deleteEntity ecs
    :<|> Service.deleteEntityByName ecs

-- | 'swaggerServer' combines the Swagger documentation with the 'EntityAPI' server.
-- It serves both the API and its Swagger documentation.
swaggerServer ::
  forall name entity.
  (Service.EntityService entity, KnownSymbol name, HasSwagger (EntityAPI name entity)) =>
  -- | Proxy for the entity name prefix.
  Proxy name ->
  -- | Proxy for the entity type.
  Proxy entity ->
  -- | Proxy for the 'EntityAPI' type.
  Proxy (EntityAPI name entity) ->
  -- | Documentation metadata.
  DocInfo ->
  -- | Cache store for the entity.
  Cache.EntityCacheStore entity ->
  -- | Combined server for the Swagger and 'EntityAPI'.
  Server (EntitySwaggerAPI name entity)
swaggerServer entityProxy entityApiProxy apiProxy docInfo cacheStore =
  let swaggerDoc = generateSwagger apiProxy docInfo
      entityApiServer = entityServer entityProxy entityApiProxy cacheStore
   in swaggerSchemaUIServer swaggerDoc :<|> entityApiServer

-- e.g.:
{-
  swaggerDoc :: Swagger
  swaggerDoc = generateSwagger apiProxy docInfo

  entityApiServer :: EntityCacheStore User -> Server UserEntityAPI
  entityApiServer = entityServer entityProxy entityApiProxy

  userServer :: EntityCacheStore User -> Server UserSwaggerAPI
  userServer s = swaggerSchemaUIServer swaggerDoc :<|> entityApiServer s
-}
