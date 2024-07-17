-- file: RandUuid.hs
-- author: Jacob Xie
-- date: 2024/07/17 22:12:27 Wednesday
-- brief:

module Main where

import Data.UUID (fromString)
import Data.UUID.V4 (nextRandom)

main :: IO ()
main = do
  uuid <- nextRandom
  print uuid

  let us = "a2964361-6a47-4e15-bbc6-9cfc3a15b116"
  print $ fromString us
