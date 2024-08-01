-- file: Schema.hs
-- author: Jacob Xie
-- date: 2024/08/01 11:16:15 Thursday
-- brief:

module PsychicEureka.Swagger.Schema where

import qualified Data.Aeson as Aeson
import qualified Data.Swagger as Swagger

-- | Helper function to drop trailing underscores from field names.
dropTrailingUnderscore :: String -> String
dropTrailingUnderscore = reverse . dropLeadingUnderscore . reverse

-- | Helper function to drop leading underscores from field names.
dropLeadingUnderscore :: String -> String
dropLeadingUnderscore ('_' : rest) = rest
dropLeadingUnderscore name = name

-- | Helper function to drop leading and trailing underscores from field names.
dropLeadingTrailingUnderscore :: String -> String
dropLeadingTrailingUnderscore = dropLeadingUnderscore . dropTrailingUnderscore

----------------------------------------------------------------------------------------------------

jsonOptDropLeadingUnderscore :: Aeson.Options
jsonOptDropLeadingUnderscore = Aeson.defaultOptions {Aeson.fieldLabelModifier = dropLeadingUnderscore}

swaggerScmDropLeadingUnderscore :: Swagger.SchemaOptions
swaggerScmDropLeadingUnderscore = Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropLeadingUnderscore}

jsonOptDropTrailingUnderscore :: Aeson.Options
jsonOptDropTrailingUnderscore = Aeson.defaultOptions {Aeson.fieldLabelModifier = dropTrailingUnderscore}

swaggerScmDropTrailingUnderscore :: Swagger.SchemaOptions
swaggerScmDropTrailingUnderscore = Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropTrailingUnderscore}

jsonOptDropLeadingTrailingUnderscore :: Aeson.Options
jsonOptDropLeadingTrailingUnderscore = Aeson.defaultOptions {Aeson.fieldLabelModifier = dropLeadingTrailingUnderscore}

swaggerScmDropLeadingTrailingUnderscore :: Swagger.SchemaOptions
swaggerScmDropLeadingTrailingUnderscore = Swagger.defaultSchemaOptions {Swagger.fieldLabelModifier = dropLeadingTrailingUnderscore}
