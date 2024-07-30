{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

-- file: Swagger.hs
-- author: Jacob Xie
-- date: 2024/07/30 13:00:11 Tuesday
-- brief:

module PsychicEureka.Swagger
  ( launch,
    app,
    DocInfo (..),
    API,
    EntityAPI,
    swaggerServer,
    entityServer,
    swaggerDoc,
  )
where

import GHC.IO.Handle (Handle)
import qualified PsychicEureka.Cache as Cache
import qualified PsychicEureka.Service as Service
import PsychicEureka.Swagger.Schema
import Servant (Application, HasServer, Proxy (..), serve)
import Servant.Swagger (HasSwagger)
import System.Info (os)
import System.Process (ProcessHandle, createProcess, shell)

launch :: Int -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launch p =
  case os of
    "mingw32" -> createProcess (shell $ "start " ++ u)
    "darwin" -> createProcess (shell $ "open " ++ u)
    _ -> createProcess (shell $ "xdg-open " ++ u)
  where
    u = "http://localhost:" <> show p <> "/swagger-ui"

app ::
  (Service.EntityService a, HasSwagger a, HasServer (API a) '[]) =>
  Proxy a ->
  DocInfo ->
  Cache.EntityCacheStore a ->
  Application
app p di ecs = serve (Proxy :: Proxy (API a)) (swaggerServer p di ecs)
