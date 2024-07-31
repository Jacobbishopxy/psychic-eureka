{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- file: Schema.hs
-- author: Jacob Xie
-- date: 2024/07/27 16:22:36 Saturday
-- brief:

module PsychicEureka.Swagger.Schema
  ( API,
    EntityAPI,
    DocInfo (..),
    swaggerDoc,
    entityServer,
    swaggerServer,
  )
where

import Control.Lens ((&), (.~), (?~))
import Data.Swagger
import Data.Text (pack)
import qualified PsychicEureka.Cache as Cache
import qualified PsychicEureka.Entity as Entity
import qualified PsychicEureka.Service as Service
import PsychicEureka.Swagger.TypeF (Desc)
import PsychicEureka.Util (Id)
import Servant
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.UI
  ( SwaggerSchemaUI,
    swaggerSchemaUIServer,
  )

type API a = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> EntityAPI a

data DocInfo = DocInfo
  { docTitle :: String,
    docVersion :: String,
    docDescription :: String
  }

-- customAPI has to be a proxy which satisfies function `entityServer`'s return type
-- e.g.: `Proxy :: Proxy (EntityAPI MyData)`
swaggerDoc :: (HasSwagger api) => Proxy api -> DocInfo -> Swagger
swaggerDoc customAPI (DocInfo t v d) =
  toSwagger customAPI
    & info . title .~ (pack t)
    & info . version .~ (pack v)
    & info . description ?~ (pack d)
    & info . license ?~ ("BSD 3.0" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")

type EntityAPI a =
  "entity_name_and_time"
    :> Summary "get entity name and the current time"
    :> Get '[PlainText] String
    :<|> "entity_name_map"
      :> Summary "name id mapping"
      :> Get '[JSON] Cache.NameIdMapping
    :<|> "entity_id_by_name"
      :> Summary "retrieve entity id by name"
      :> QueryParam' '[Required, Desc String "entity name"] "name" String
      :> Get '[JSON] Id
    :<|> "entity"
      :> Summary "retrieve entity identified by :id"
      :> Capture' '[Desc Id "unique identifier"] ":id" Id
      :> Get '[JSON] a
    :<|> "entity_by_name"
      :> Summary "retrieve entity by name"
      :> QueryParam' '[Required, Desc String "entity name"] "name" String
      :> Get '[JSON] a
    :<|> "entity_all"
      :> Summary "retrieve all entities"
      :> Get '[JSON] [a]
    :<|> "entity"
      :> Summary "store a new entity"
      :> ReqBody '[JSON] (Entity.EntityInput a)
      :> Post '[JSON] a
    :<|> "entity"
      :> Summary "modify an existing entity"
      :> Capture' '[Desc Id "unique identifier"] ":id" Id
      :> ReqBody '[JSON] (Entity.EntityInput a)
      :> Put '[JSON] a
    :<|> "entity_by_name"
      :> Summary "modify an existing entity by name"
      :> QueryParam' '[Required, Desc String "entity name"] "name" String
      :> ReqBody '[JSON] (Entity.EntityInput a)
      :> Put '[JSON] a
    :<|> "entity"
      :> Summary "delete an existing entity"
      :> Capture' '[Desc Id "unique identifier"] ":id" Id
      :> Delete '[JSON] a
    :<|> "entity_by_name"
      :> Summary "delete an existing entity by name"
      :> QueryParam' '[Required, Desc String "entity name"] "name" String
      :> Delete '[JSON] a

entityServer ::
  (Service.EntityService a) =>
  Proxy a ->
  Cache.EntityCacheStore a ->
  Server (EntityAPI a)
entityServer p ecs =
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

-- A combined function works like below
{-
  let userType = Proxy :: Proxy User
      entityApi = Proxy :: Proxy (EntityAPI User)
      api = Proxy :: Proxy (API User)
      sd = swaggerDoc entityApi docInfo
      es = entityServer userType store
      ss = swaggerSchemaUIServer sd :<|> es :: Server (API User)
      app' = serve api ss

  run 8080 app'
-}
swaggerServer ::
  (Service.EntityService a, HasSwagger (EntityAPI a)) =>
  Proxy a ->
  Proxy (EntityAPI a) ->
  DocInfo ->
  Cache.EntityCacheStore a ->
  Server (API a)
swaggerServer p pe di ecs =
  swaggerSchemaUIServer (swaggerDoc pe di) :<|> (entityServer p ecs)
