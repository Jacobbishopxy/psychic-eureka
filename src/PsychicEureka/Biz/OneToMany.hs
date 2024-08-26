{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- file: OneToMany.hs
-- author: Jacob Xie
-- date: 2024/08/12 13:42:45 Monday
-- brief:

module PsychicEureka.Biz.OneToMany
  ( CacheOneToMany (..),
    OneToMany (..),
  )
where

import Control.Concurrent (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (throw)
import Data.Aeson (FromJSON, ToJSON, encodeFile)
import Data.Data (Proxy (..), typeRep)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import qualified PsychicEureka.Cache as Cache
import qualified PsychicEureka.Entity as Entity
import PsychicEureka.Error (EurekaError (..))
import qualified PsychicEureka.Internal.Util as Util
import PsychicEureka.Util (Id)

----------------------------------------------------------------------------------------------------
-- Types & Newtype
----------------------------------------------------------------------------------------------------

type MainId = Id

type RefId = Id

type MainName = String

type RefName = String

type RefO2M = Map.Map MainId [RefId]

newtype RefRelationO2MData = RefRelationO2MData RefO2M
  deriving (Generic, ToJSON, FromJSON)

type RefRelationO2M = MVar RefRelationO2MData

data CacheOneToMany a b = CacheOneToMany
  { cacheStoreMain :: Cache.EntityCacheStore a,
    cacheStoreSub :: Cache.EntityCacheStore b,
    refRelation :: RefRelationO2M
  }

----------------------------------------------------------------------------------------------------
-- OneToMany
----------------------------------------------------------------------------------------------------

class (Cache.EntityCache a, Cache.EntityCache b) => OneToMany a b where
  ----------------------------------------------------------------------------------------------------
  -- default impl
  ----------------------------------------------------------------------------------------------------

  -- default persistence directory
  refRelationPersist :: Proxy a -> Proxy b -> FilePath
  refRelationPersist _ _ = "./data/o2m." <> show (typeRep a) <> "." <> show (typeRep b) <> ".json"
    where
      a = Proxy @a
      b = Proxy @b

  construct :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheOneToMany a b)
  construct ca cb = do
    defaultO2M <- defaultRefRelationO2MData ca
    ref <- Util.decodeFileOrCreate (refRelationPersist a b) defaultO2M
    ref' <- newMVar ref
    return $ CacheOneToMany ca cb ref'
    where
      a = Proxy @a
      b = Proxy @b

  constructWithoutRef :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheOneToMany a b)
  constructWithoutRef ca cb = do
    defaultO2M <- defaultRefRelationO2MData ca
    _ <- encodeFile (refRelationPersist a b) defaultO2M
    ref' <- newMVar defaultO2M
    return $ CacheOneToMany ca cb ref'
    where
      a = Proxy @a
      b = Proxy @b

  getRefMap :: CacheOneToMany a b -> IO RefO2M
  getRefMap = readRefO2M . refRelation

  isIdInKey :: CacheOneToMany a b -> MainId -> IO Bool
  isIdInKey (CacheOneToMany _ _ r) mi =
    readRefO2M r >>= return . Map.member mi

  isIdInValue :: CacheOneToMany a b -> MainId -> RefId -> IO Bool
  isIdInValue (CacheOneToMany _ _ r) mi ri = do
    readRefO2M r >>= \m ->
      case Map.lookup mi m of
        Nothing -> return False
        Just e -> return $ ri `elem` e

  getAllRefIds :: CacheOneToMany a b -> MainId -> IO [RefId]
  getAllRefIds (CacheOneToMany _ _ r) mi =
    readRefO2M r >>= \m ->
      case Map.lookup mi m of
        Nothing -> throw $ IdNotFound mi
        Just e -> return e

  getAllRefIdsByName :: CacheOneToMany a b -> MainName -> IO [RefId]
  getAllRefIdsByName c@(CacheOneToMany ca _ _) mn =
    Cache.getIdByName ca mn >>= getAllRefIds c

  getAllRef :: CacheOneToMany a b -> MainId -> IO [b]
  getAllRef c@(CacheOneToMany _ cb _) mi =
    getAllRefIds c mi >>= Cache.retrieveMany cb

  getAllRefByName :: CacheOneToMany a b -> MainName -> IO [b]
  getAllRefByName c@(CacheOneToMany ca _ _) mn =
    Cache.getIdByName ca mn >>= getAllRef c

  getManyRef :: CacheOneToMany a b -> MainId -> [RefId] -> IO [b]
  getManyRef c@(CacheOneToMany _ cb _) mi ris =
    getAllRefIds c mi >>= Cache.retrieveMany cb . filter (`elem` ris)

  saveRef :: CacheOneToMany a b -> MainId -> Entity.EntityInput b -> IO b
  saveRef c@(CacheOneToMany _ cb r) mi inp =
    isIdInKey c mi >>= \case
      True -> do
        sb <- Cache.save cb inp
        let newRefId = Entity.getId sb
        newR <- modifyRefO2M r $ insertRefId mi newRefId
        saveRefO2M (refRelationPersist a b) newR
        return sb
      False -> throw $ IdNotFound mi
    where
      a = Proxy @a
      b = Proxy @b

  saveRefByName :: CacheOneToMany a b -> MainName -> Entity.EntityInput b -> IO b
  saveRefByName c@(CacheOneToMany ca _ _) mn inp =
    Cache.getIdByName ca mn >>= \mi -> saveRef c mi inp

  updateRef :: CacheOneToMany a b -> MainId -> RefId -> Entity.EntityInput b -> IO b
  updateRef c@(CacheOneToMany _ cb _) mi ri inp =
    isIdInValue c mi ri >>= \case
      True -> Cache.update cb ri inp
      False -> throw $ IdNotFound ri

  updateRefByName :: CacheOneToMany a b -> MainName -> RefName -> Entity.EntityInput b -> IO b
  updateRefByName c@(CacheOneToMany ca cb _) mn rn inp = do
    mi <- Cache.getIdByName ca mn
    ri <- Cache.getIdByName cb rn
    updateRef c mi ri inp

  removeRef :: CacheOneToMany a b -> MainId -> RefId -> IO b
  removeRef c@(CacheOneToMany _ cb r) mi ri =
    isIdInKey c mi >>= \case
      True -> do
        rb <- Cache.remove cb ri
        newR <- modifyRefO2M r $ removeRefId mi ri
        saveRefO2M (refRelationPersist a b) newR
        return rb
      False -> throw $ IdNotFound ri
    where
      a = Proxy @a
      b = Proxy @b

  removeRefByName :: CacheOneToMany a b -> MainName -> RefName -> IO b
  removeRefByName c@(CacheOneToMany ca cb _) mn rn = do
    mi <- Cache.getIdByName ca mn
    ri <- Cache.getIdByName cb rn
    removeRef c mi ri

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

refO2M :: RefRelationO2MData -> RefO2M
refO2M (RefRelationO2MData d) = d

defaultRefRelationO2MData :: Cache.EntityCacheStore a -> IO RefRelationO2MData
defaultRefRelationO2MData m = do
  (_, m') <- readMVar m
  let res = Map.map (const []) m'
  return $ RefRelationO2MData res

readRefO2M :: RefRelationO2M -> IO RefO2M
readRefO2M r = readMVar r >>= return . refO2M

modifyRefO2M :: RefRelationO2M -> (RefRelationO2MData -> RefRelationO2MData) -> IO RefRelationO2MData
modifyRefO2M r fn = modifyMVar r $ \d -> let newD = fn d in return (newD, newD)

saveRefO2M :: FilePath -> RefRelationO2MData -> IO ()
saveRefO2M = encodeFile

insertRefId :: MainId -> RefId -> RefRelationO2MData -> RefRelationO2MData
insertRefId mi ri (RefRelationO2MData m) =
  RefRelationO2MData um
  where
    um = Map.insertWith (++) mi [ri] m

removeRefId :: MainId -> RefId -> RefRelationO2MData -> RefRelationO2MData
removeRefId mi ri (RefRelationO2MData m) =
  RefRelationO2MData um
  where
    um = Map.update fn mi m
    fn :: [RefId] -> Maybe [RefId]
    fn ris =
      let newRis = filter (/= ri) ris
       in if null newRis then Nothing else Just newRis
