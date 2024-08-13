-- file: Common.hs
-- author: Jacob Xie
-- date: 2024/08/12 13:56:19 Monday
-- brief:

module PsychicEureka.Biz.Common where

import PsychicEureka.Util (Id)

class RefEntity a where
  getRef :: a -> Id
