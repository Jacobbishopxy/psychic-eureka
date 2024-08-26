{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- file: ManyToMany.hs
-- author: Jacob Xie
-- date: 2024/08/26 08:27:41 Monday
-- brief:

module PsychicEureka.Biz.ManyToMany
  ( CacheManyToMany (..),
    ManyToMany (..),
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
import PsychicEureka.Error (EurekaError (IdNotFound))
import qualified PsychicEureka.Internal.Util as Util
import PsychicEureka.Util (Id)

----------------------------------------------------------------------------------------------------
-- Types & Newtype
----------------------------------------------------------------------------------------------------

type LeftId = Id

type RightId = Id

type LeftName = String

type RightName = String

type RefM2ML = Map.Map LeftId [RightId]

type RefM2MR = Map.Map RightId [LeftId]

data RefRelationM2MData = RefRelationM2MData
  { l2r :: RefM2ML,
    r2l :: RefM2MR
  }
  deriving (Generic, ToJSON, FromJSON)

type RefRelationM2M = MVar RefRelationM2MData

data CacheManyToMany a b = CacheManyToMany
  { cacheStoreLeft :: Cache.EntityCacheStore a,
    cacheStoreRight :: Cache.EntityCacheStore b,
    refRelation :: RefRelationM2M
  }

----------------------------------------------------------------------------------------------------
-- ManyToMany
----------------------------------------------------------------------------------------------------

class (Cache.EntityCache a, Cache.EntityCache b) => ManyToMany a b where
  ----------------------------------------------------------------------------------------------------
  -- default impl
  ----------------------------------------------------------------------------------------------------

  -- default persistence directory
  refRelationPersist :: Proxy a -> Proxy b -> FilePath
  refRelationPersist _ _ = "./data/m2m." <> show (typeRep a) <> "." <> show (typeRep b) <> ".json"
    where
      a = Proxy @a
      b = Proxy @b

  construct :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheManyToMany a b)
  construct ca cb = do
    defaultM2M <- defaultRefRelationM2MData ca cb
    ref <- Util.decodeFileOrCreate (refRelationPersist a b) defaultM2M
    ref' <- newMVar ref
    return $ CacheManyToMany ca cb ref'
    where
      a = Proxy @a
      b = Proxy @b

  constructWithoutRef :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO (CacheManyToMany a b)
  constructWithoutRef ca cb = do
    defaultM2M <- defaultRefRelationM2MData ca cb
    _ <- encodeFile (refRelationPersist a b) defaultM2M
    ref' <- newMVar defaultM2M
    return $ CacheManyToMany ca cb ref'
    where
      a = Proxy @a
      b = Proxy @b

  getRefMap :: CacheManyToMany a b -> IO RefRelationM2MData
  getRefMap = readRefM2M . refRelation

  getRefMapL :: CacheManyToMany a b -> IO RefM2ML
  getRefMapL (CacheManyToMany _ _ r) = readRefM2M r >>= return . l2r

  getRefMapR :: CacheManyToMany a b -> IO RefM2MR
  getRefMapR (CacheManyToMany _ _ r) = readRefM2M r >>= return . r2l

  isIdInKeyL :: CacheManyToMany a b -> LeftId -> IO Bool
  isIdInKeyL (CacheManyToMany _ _ r) li =
    readRefM2ML r >>= return . Map.member li

  isIdInKeyR :: CacheManyToMany a b -> RightId -> IO Bool
  isIdInKeyR (CacheManyToMany _ _ r) ri =
    readRefM2MR r >>= return . Map.member ri

  isIdInValueL :: CacheManyToMany a b -> LeftId -> RightId -> IO Bool
  isIdInValueL (CacheManyToMany _ _ r) li ri =
    readRefM2ML r >>= \m ->
      case Map.lookup li m of
        Nothing -> return False
        Just e -> return $ ri `elem` e

  isIdInValueR :: CacheManyToMany a b -> RightId -> LeftId -> IO Bool
  isIdInValueR (CacheManyToMany _ _ r) ri li =
    readRefM2MR r >>= \m ->
      case Map.lookup ri m of
        Nothing -> return False
        Just e -> return $ li `elem` e

  getAllRefIdsL :: CacheManyToMany a b -> LeftId -> IO [RightId]
  getAllRefIdsL (CacheManyToMany _ _ r) li =
    readRefM2ML r >>= \m ->
      case Map.lookup li m of
        Nothing -> throw $ IdNotFound li
        Just e -> return e

  getAllRefIdsR :: CacheManyToMany a b -> RightId -> IO [LeftId]
  getAllRefIdsR (CacheManyToMany _ _ r) ri =
    readRefM2MR r >>= \m ->
      case Map.lookup ri m of
        Nothing -> throw $ IdNotFound ri
        Just e -> return e

  getAllRefIdsByNameL :: CacheManyToMany a b -> LeftName -> IO [RightId]
  getAllRefIdsByNameL c@(CacheManyToMany ca _ _) ln =
    Cache.getIdByName ca ln >>= getAllRefIdsL c

  getAllRefIdsByNameR :: CacheManyToMany a b -> RightName -> IO [LeftId]
  getAllRefIdsByNameR c@(CacheManyToMany _ cb _) rn =
    Cache.getIdByName cb rn >>= getAllRefIdsR c

  getAllRefL :: CacheManyToMany a b -> LeftId -> IO [b]
  getAllRefL c@(CacheManyToMany _ cb _) li =
    getAllRefIdsL c li >>= Cache.retrieveMany cb

  getAllRefR :: CacheManyToMany a b -> RightId -> IO [a]
  getAllRefR c@(CacheManyToMany ca _ _) ri =
    getAllRefIdsR c ri >>= Cache.retrieveMany ca

  getAllRefByNameL :: CacheManyToMany a b -> LeftName -> IO [b]
  getAllRefByNameL c@(CacheManyToMany ca _ _) ln =
    Cache.getIdByName ca ln >>= getAllRefL c

  getAllRefByNameR :: CacheManyToMany a b -> RightName -> IO [a]
  getAllRefByNameR c@(CacheManyToMany _ cb _) rn =
    Cache.getIdByName cb rn >>= getAllRefR c

  getManyRefL :: CacheManyToMany a b -> LeftId -> [RightId] -> IO [b]
  getManyRefL c@(CacheManyToMany _ cb _) li ris =
    getAllRefIdsL c li >>= Cache.retrieveMany cb . filter (`elem` ris)

  getManyRefR :: CacheManyToMany a b -> RightId -> [LeftId] -> IO [a]
  getManyRefR c@(CacheManyToMany ca _ _) ri lis =
    getAllRefIdsR c ri >>= Cache.retrieveMany ca . filter (`elem` lis)

  saveRefL :: CacheManyToMany a b -> LeftId -> Entity.EntityInput b -> IO b
  saveRefL c@(CacheManyToMany _ cb r) li inp =
    isIdInKeyL c li >>= \case
      True -> do
        sb <- Cache.save cb inp
        let newRefId = Entity.getId sb
        res <- modifyRefM2M r $ insertRefL li newRefId
        saveRefM2M (refRelationPersist a b) res
        return sb
      False -> throw $ IdNotFound li
    where
      a = Proxy @a
      b = Proxy @b

  saveRefR :: CacheManyToMany a b -> RightId -> Entity.EntityInput a -> IO a
  saveRefR c@(CacheManyToMany ca _ r) ri inp =
    isIdInKeyR c ri >>= \case
      True -> do
        sa <- Cache.save ca inp
        let newRefId = Entity.getId sa
        res <- modifyRefM2M r $ insertRefR ri newRefId
        saveRefM2M (refRelationPersist a b) res
        return sa
      False -> throw $ IdNotFound ri
    where
      a = Proxy @a
      b = Proxy @b

  saveRefByNameL :: CacheManyToMany a b -> LeftName -> Entity.EntityInput b -> IO b
  saveRefByNameL c@(CacheManyToMany ca _ _) ln inp =
    Cache.getIdByName ca ln >>= \li -> saveRefL c li inp

  saveRefByNameR :: CacheManyToMany a b -> RightName -> Entity.EntityInput a -> IO a
  saveRefByNameR c@(CacheManyToMany _ cb _) rn inp =
    Cache.getIdByName cb rn >>= \ri -> saveRefR c ri inp

  updateRefL :: CacheManyToMany a b -> LeftId -> RightId -> Entity.EntityInput b -> IO b
  updateRefL c@(CacheManyToMany _ cb _) li ri inp =
    isIdInValueL c li ri >>= \case
      True -> Cache.update cb ri inp
      False -> throw $ IdNotFound ri

  updateRefR :: CacheManyToMany a b -> RightId -> LeftId -> Entity.EntityInput a -> IO a
  updateRefR c@(CacheManyToMany ca _ _) ri li inp =
    isIdInValueR c ri li >>= \case
      True -> Cache.update ca li inp
      False -> throw $ IdNotFound li

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

defaultRefRelationM2MData :: Cache.EntityCacheStore a -> Cache.EntityCacheStore b -> IO RefRelationM2MData
defaultRefRelationM2MData ml mr = do
  (_, ml') <- readMVar ml
  (_, mr') <- readMVar mr
  let l2r' = Map.map (const []) ml'
      r2l' = Map.map (const []) mr'
  return $ RefRelationM2MData l2r' r2l'

readRefM2M :: RefRelationM2M -> IO RefRelationM2MData
readRefM2M = readMVar

readRefM2ML :: RefRelationM2M -> IO RefM2ML
readRefM2ML r = readMVar r >>= return . l2r

readRefM2MR :: RefRelationM2M -> IO RefM2MR
readRefM2MR r = readMVar r >>= return . r2l

modifyRefM2M :: RefRelationM2M -> (RefRelationM2MData -> RefRelationM2MData) -> IO RefRelationM2MData
modifyRefM2M r fn = modifyMVar r $ \d -> let newD = fn d in return (newD, newD)

modifyRefM2ML :: RefRelationM2M -> (RefM2ML -> RefM2ML) -> IO RefM2ML
modifyRefM2ML r fn = modifyMVar r $ \d ->
  let newL = fn (l2r d)
      newD = RefRelationM2MData newL (r2l d)
   in return (newD, newL)

modifyRefM2MR :: RefRelationM2M -> (RefM2MR -> RefM2MR) -> IO RefM2MR
modifyRefM2MR r fn = modifyMVar r $ \d ->
  let newR = fn (r2l d)
      newD = RefRelationM2MData (l2r d) newR
   in return (newD, newR)

insertRefL :: LeftId -> RightId -> RefRelationM2MData -> RefRelationM2MData
insertRefL li ri d =
  let ld = l2r d
      newLd = Map.insertWith (++) li [ri] ld
   in RefRelationM2MData newLd (r2l d)

insertRefR :: RightId -> LeftId -> RefRelationM2MData -> RefRelationM2MData
insertRefR ri li d =
  let rd = r2l d
      newRd = Map.insertWith (++) ri [li] rd
   in RefRelationM2MData (l2r d) newRd

saveRefM2M :: FilePath -> RefRelationM2MData -> IO ()
saveRefM2M = encodeFile

removeRefL :: LeftId -> RightId -> RefRelationM2MData -> RefRelationM2MData
removeRefL li ri d =
  let ld = l2r d
      newLd = Map.update (removeIdFromValue ri) li ld
   in RefRelationM2MData newLd (r2l d)

removeRefR :: RightId -> LeftId -> RefRelationM2MData -> RefRelationM2MData
removeRefR ri li d =
  let rd = r2l d
      newRd = Map.update (removeIdFromValue li) ri rd
   in RefRelationM2MData (r2l d) newRd

removeIdFromValue :: Id -> [Id] -> Maybe [Id]
removeIdFromValue i ris =
  let newIds = filter (/= i) ris
   in if null newIds then Nothing else Just newIds
