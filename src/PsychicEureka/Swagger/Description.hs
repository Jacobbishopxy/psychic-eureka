{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- file: Description.hs
-- author: Jacob Xie
-- date: 2024/07/27 16:22:57 Saturday
-- brief:

module PsychicEureka.Swagger.Description where

import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (AppendSymbol, Symbol)
import Servant (Description)

type family TypeName (x :: Type) :: Symbol where
  TypeName Int = "integer"
  TypeName String = "string"
  TypeName x = GenericTypeName x (Rep x ())

type family GenericTypeName t (r :: Type) :: Symbol where
  GenericTypeName t (D1 ('MetaData name mod pkg nt) f x) = name

type Desc t n = Description (AppendSymbol (TypeName t) (AppendSymbol " | " n))
