{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data
       ( Reaction(..)
       , Molecule(..)
       , Catalyst(..)
       , PRODUCT_FROM(..)
       , ACCELERATE(..)
       , FullReaction(..)
       , toReaction)
       where

import           Database.Bolt
import           Data.Text as T (Text, pack)
import           Data.Maybe     (fromJust)
import           Data.Map as M  (fromList, Map, empty, union, notMember, (!))
import           Control.Monad  ((>=>))

data Molecule = Molecule { id :: Int
                         , smilesM :: Text
                         , iupacName :: Text
                         } deriving (Show, Eq)

data Reaction = Reaction { id :: Int
                         , nameR :: Text
                         } deriving (Show, Eq)

data Catalyst = Catalyst { id :: Int
                         , smilesC :: Text
                         , nameC :: Maybe Text
                         } deriving (Show, Eq)

data PRODUCT_FROM = PRODUCT_FROM { amount :: Double } deriving (Show, Eq)

data ACCELERATE = ACCELERATE { temperature :: Double
                             , pressure :: Double
                             } deriving (Show, Eq)

data FullReaction = FullReaction { firstIn :: Molecule
                                 , secondIn :: Molecule
                                 , out :: Molecule
                                 , catalyst :: Catalyst
                                 , reaction :: Reaction
                                 , productFrom :: PRODUCT_FROM
                                 , accelerate :: ACCELERATE
                                 } deriving (Show, Eq)

toReaction :: Monad m => Value -> m Reaction
toReaction v = do node <- exact v
                  let props = nodeProps node
                  let identity = nodeIdentity node
                  name <- (props `at` "name")  >>= exact
                  return $ Reaction identity name







