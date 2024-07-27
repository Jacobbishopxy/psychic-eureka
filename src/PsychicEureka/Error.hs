{-# LANGUAGE DeriveAnyClass #-}

-- file: Error.hs
-- author: Jacob Xie
-- date: 2024/07/27 16:15:06 Saturday
-- brief:

module PsychicEureka.Error where

import Control.Exception (Exception)

data EurekaError
  = EntityNotFound String
  | EntityAlreadyExists String
  | InternalError String
  deriving (Show, Exception)
