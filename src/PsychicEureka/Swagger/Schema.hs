{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- file: Schema.hs
-- author: Jacob Xie
-- date: 2024/07/27 16:22:36 Saturday
-- brief:

module PsychicEureka.Swagger.Schema
  ( API,
    DocInfo (..),
    swaggerDoc,
    launch,
  )
where

import Control.Lens ((&), (.~), (?~))
import Data.Swagger
import Data.Text (pack)
import GHC.IO.Handle (Handle)
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import System.Info (os)
import System.Process (ProcessHandle, createProcess, shell)

type API a = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> a

data DocInfo = DocInfo
  { docTitle :: String,
    docVersion :: String,
    docDescription :: String
  }

swaggerDoc :: (HasSwagger api) => Proxy api -> DocInfo -> Swagger
swaggerDoc customAPI (DocInfo t v d) =
  toSwagger customAPI
    & info . title .~ (pack t)
    & info . version .~ (pack v)
    & info . description ?~ (pack d)
    & info . license ?~ ("BSD 3.0" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")

launch :: Int -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launch p =
  case os of
    "mingw32" -> createProcess (shell $ "start " ++ u)
    "darwin" -> createProcess (shell $ "open " ++ u)
    _ -> createProcess (shell $ "xdg-open " ++ u)
  where
    u = "http://localhost:" <> show p <> "/swagger-ui"
