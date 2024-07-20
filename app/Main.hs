-- file: Main.hs
-- author: Jacob Xie
-- date: 2024/07/15 23:27:23 Monday
-- brief: https://github.com/thma/ServantRestService

module Main where

import SwaggerService (up)

main :: IO ()
main = up 8080
