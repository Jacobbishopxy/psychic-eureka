{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- file: Description.hs
-- author: Jacob Xie
-- date: 2024/07/17 20:11:19 Wednesday
-- brief:

module Description where

import Data.Kind (Type)
import GHC.Generics (D1, Generic (Rep), Meta (MetaData))
import GHC.TypeLits (AppendSymbol, Symbol)
import Servant.API (Description)

type family TypeName (x :: Type) :: Symbol where
  TypeName Int = "integer"
  TypeName String = "string"
  TypeName x = GenericTypeName x (Rep x ())

type family GenericTypeName t (r :: Type) :: Symbol where
  GenericTypeName t (D1 ('MetaData name mod pkg nt) f x) = name

type Desc t n = Description (AppendSymbol (TypeName t) (AppendSymbol " | " n))
