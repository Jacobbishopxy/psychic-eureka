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
    generateSimpleSwagger,
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
  { -- | Title of the API documentation.
    docTitle :: String,
    -- | Version of the API.
    docVersion :: String,
    -- | Description of the API.
    docDescription :: String,
    -- | List of tags and their descriptions for categorizing operations.
    docTags :: [(String, String)]
  }

generateSimpleSwagger ::
  (HasSwagger a) =>
  Proxy a ->
  (String, String) ->
  Swagger
generateSimpleSwagger apiProxy (tag, tagDesc) =
  toSwagger apiProxy & applyTags [Tag (pack tag) (Just $ pack tagDesc) Nothing]

generateSwagger ::
  (HasSwagger a) =>
  -- | Proxy for the API type.
  Proxy a ->
  -- | Documentation metadata.
  DocInfo ->
  -- | Generated Swagger documentation.
  Swagger
generateSwagger apiProxy (DocInfo t v d tg) =
  let swg =
        toSwagger apiProxy
          & info . title .~ pack t -- Set the title of the API.
          & info . version .~ pack v -- Set the version of the API.
          & info . description ?~ pack d -- Set the description of the API.
          & info . license ?~ ("BSD 3.0" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause") -- Set the license information.
   in if not (null tg)
        then applyTags' tg swg
        else swg

applyTags' :: [(String, String)] -> Swagger -> Swagger
applyTags' tg swagger =
  let newTags = [Tag (pack tag) (Just $ pack tagDesc) Nothing | (tag, tagDesc) <- tg]
   in swagger & applyTags newTags

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
    :<|> Service.idExists ecs
    :<|> Service.getEntityIdByName ecs
    :<|> Service.getEntity ecs
    :<|> Service.getEntityByName ecs
    :<|> Service.getEntities' ecs
    :<|> Service.getEntitiesByName' ecs
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
