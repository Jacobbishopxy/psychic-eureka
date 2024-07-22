{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- file: Caching.hs
-- author: Jacob Xie
-- date: 2024/07/22 09:56:28 Monday
-- brief:

module Caching where

import Control.Exception (Exception, throw)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Util

----------------------------------------------------------------------------------------------------
-- Caching
----------------------------------------------------------------------------------------------------

data CachingErr
  = CacheNotFound String
  | CacheAlreadyExists String
  | InternalError String
  deriving (Show, Exception)

-- used for mapping of `name` to `Id` (UserStore), `name` to `[Id]` (PostingStore)
newtype CachingStore a = CachingStore (IORef (Map.Map String a))

initCaching :: [(String, a)] -> IO (CachingStore a)
initCaching l = newIORef (Map.fromList l) >>= return . CachingStore

getKeys :: CachingStore a -> IO [String]
getKeys (CachingStore ref) =
  readIORef ref >>= return . Map.keys

getValue :: CachingStore a -> String -> IO (Maybe a)
getValue (CachingStore ref) k =
  readIORef ref >>= return . Map.lookup k

insertValue :: CachingStore a -> String -> a -> IO ()
insertValue (CachingStore ref) k v =
  getValue (CachingStore ref) k >>= \case
    Just _ -> throw $ CacheAlreadyExists $ "cache record already exists: " <> k
    Nothing -> modifyIORef ref $ Map.insert k v

updateValue :: CachingStore a -> String -> a -> IO ()
updateValue (CachingStore ref) k v =
  getValue (CachingStore ref) k >>= \case
    Nothing -> throw $ CacheNotFound $ "cache record not found: " <> k
    Just _ -> modifyIORef ref $ Map.insert k v

deleteKey :: CachingStore a -> String -> IO a
deleteKey (CachingStore ref) k =
  getValue (CachingStore ref) k >>= \case
    Nothing -> throw $ CacheNotFound $ "cache record not found: " <> k
    Just v -> modifyIORef ref (Map.delete k) >> return v

----------------------------------------------------------------------------------------------------

type CachingStoreSetId = CachingStore (Set.Set Id)

setIdIsIn :: CachingStoreSetId -> String -> Id -> IO Bool
setIdIsIn = undefined

insertId :: CachingStoreSetId -> String -> Id -> IO ()
insertId = undefined

deleteId :: CachingStoreSetId -> String -> Id -> IO ()
deleteId = undefined
