{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Database.Bolt
import Data.Default

someFunc :: IO ()
someFunc = do
  pipe <- connect $ def { user = "neo4j", password = "22dron10"}
  close pipe



