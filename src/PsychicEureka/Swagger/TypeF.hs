{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- file: TypeF.hs
-- author: Jacob Xie
-- date: 2024/07/27 16:22:57 Saturday
-- brief:

module PsychicEureka.Swagger.TypeF
  ( Desc,
    EntityAPI,
  )
where

import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (AppendSymbol, Symbol)
import PsychicEureka (Id)
import qualified PsychicEureka.Cache as Cache
import qualified PsychicEureka.Entity as Entity
import Servant

----------------------------------------------------------------------------------------------------
-- Desc

type family TypeName (x :: Type) :: Symbol where
  TypeName Int = "integer"
  TypeName String = "string"
  TypeName x = GenericTypeName x (Rep x ())

type family GenericTypeName t (r :: Type) :: Symbol where
  GenericTypeName t (D1 ('MetaData name mod pkg nt) f x) = name

type Desc t n = Description (AppendSymbol (TypeName t) (AppendSymbol " | " n))

----------------------------------------------------------------------------------------------------
-- API

type family (:<>:) (s1 :: Symbol) (s2 :: Symbol) :: Symbol where
  s1 :<>: s2 = AppendSymbol s1 s2

type family EntityAPI (prefix :: Symbol) a where
  EntityAPI prefix a =
    (prefix :<>: "_name_and_time")
      :> Summary ("get " :<>: prefix :<>: " name and the current time")
      :> Get '[PlainText] String
      :<|> (prefix :<>: "_name_map")
        :> Summary (prefix :<>: " name id mapping")
        :> Get '[JSON] Cache.NameIdMapping
      :<|> (prefix :<>: "_id_by_name")
        :> Summary ("retrieve " :<>: prefix :<>: " id by name")
        :> QueryParam' '[Required, Desc String (prefix :<>: " name")] "name" String
        :> Get '[JSON] Id
      :<|> prefix
        :> Summary ("retrieve " :<>: prefix :<>: " identified by :id")
        :> Capture' '[Desc Id "unique identifier"] ":id" Id
        :> Get '[JSON] a
      :<|> (prefix :<>: "_by_name")
        :> Summary ("retrieve " :<>: prefix :<>: " by name")
        :> QueryParam' '[Required, Desc String (prefix :<>: " name")] "name" String
        :> Get '[JSON] a
      :<|> (prefix :<>: "_all")
        :> Summary ("retrieve all " :<>: prefix)
        :> Get '[JSON] [a]
      :<|> prefix
        :> Summary ("store a new " :<>: prefix)
        :> ReqBody '[JSON] (Entity.EntityInput a)
        :> Post '[JSON] a
      :<|> prefix
        :> Summary ("modify an existing " :<>: prefix)
        :> Capture' '[Desc Id "unique identifier"] ":id" Id
        :> ReqBody '[JSON] (Entity.EntityInput a)
        :> Put '[JSON] a
      :<|> (prefix :<>: "_by_name")
        :> Summary ("modify an existing " :<>: prefix :<>: " by name")
        :> QueryParam' '[Required, Desc String (prefix :<>: " name")] "name" String
        :> ReqBody '[JSON] (Entity.EntityInput a)
        :> Put '[JSON] a
      :<|> prefix
        :> Summary ("delete an existing " :<>: prefix)
        :> Capture' '[Desc Id "unique identifier"] ":id" Id
        :> Delete '[JSON] a
      :<|> (prefix :<>: "_by_name")
        :> Summary ("delete an existing " :<>: prefix :<>: " by name")
        :> QueryParam' '[Required, Desc String (prefix :<>: " name")] "name" String
        :> Delete '[JSON] a
