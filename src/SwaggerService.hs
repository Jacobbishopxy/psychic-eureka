{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- file: SwaggerService.hs
-- author: Jacob Xie
-- date: 2024/07/17 21:52:34 Wednesday
-- brief:

module SwaggerService where

import Control.Lens (mapped, (&), (.~), (?~))
import Data.Aeson (ToJSON (toJSON))
import Data.Swagger
import Entities (User (User), UserData (UserData))
import EntityService (UserAPI, userAPI, userServer)
import GHC.IO.Handle (Handle)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Application,
    Proxy (..),
    Server,
    serve,
    type (:<|>) (..),
  )
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import System.Info (os)
import System.Process (ProcessHandle, createProcess, shell)
import Util (defaultUTCTime, mockId)

----------------------------------------------------------------------------------------------------
-- User
----------------------------------------------------------------------------------------------------

instance ToSchema User where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "This is a User API (tm)"
      & mapped . schema . example ?~ toJSON (User mockId "JacobX" "xy@dev.com" defaultUTCTime defaultUTCTime)

instance ToSchema UserData where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "Used for create/modify a User"
      & mapped . schema . example ?~ toJSON (UserData "XY" "xy@prod.com")

----------------------------------------------------------------------------------------------------
-- Posting
----------------------------------------------------------------------------------------------------

-- TODO

----------------------------------------------------------------------------------------------------
-- Server
----------------------------------------------------------------------------------------------------

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger userAPI
    & info . title .~ "User API"
    & info . version .~ "0.1"
    & info . description ?~ "This is an API that tests swagger integration"
    & info . license ?~ ("BSD 3.0" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> UserAPI

api :: Proxy API
api = Proxy

server :: Server API
server = swaggerSchemaUIServer swaggerDoc :<|> userServer

app :: Application
app = serve api server

launch :: Int -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launch p =
  case os of
    "mingw32" -> createProcess (shell $ "start " ++ u)
    "darwin" -> createProcess (shell $ "open " ++ u)
    _ -> createProcess (shell $ "xdg-open " ++ u)
  where
    u = "http://localhost:" <> show p <> "/swagger-ui"

up :: Int -> IO ()
up p = do
  putStrLn $ "Server start, listening on " <> show p
  _ <- launch p
  run p app
