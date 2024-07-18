{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- file: SwaggerService.hs
-- author: Jacob Xie
-- date: 2024/07/17 21:52:34 Wednesday
-- brief:

module SwaggerService where

import Control.Lens
import Data.Aeson
import Data.Swagger
import Data.UUID (UUID, fromString)
import Entities
import EntityService
import GHC.IO.Handle (Handle)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import System.Info (os)
import System.Process (ProcessHandle, createProcess, shell)

mockUid :: UUID
mockUid =
  case fromString "a2964361-6a47-4e15-bbc6-9cfc3a15b116" of
    Just u -> u
    Nothing -> error "wrong UUID string"

instance ToSchema User where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "This is a User API (tm)"
      & mapped . schema . example ?~ toJSON (User "1" "Max Muster" "mm@muster.com")

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

up :: IO ()
up = do
  let p = 8080
  putStrLn $ "Server start, listening on " <> show p
  _ <- launch p
  run p app
