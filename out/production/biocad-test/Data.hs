{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data where

import Database.Bolt
import Data.Text (Text)

data Molecule = Molecule { id :: Int
                         , smiles :: Text
                         , iupacName :: Text
                         }

data Reaction = Reaction { id :: Int
                         , name :: Text
                         } deriving (Show, Eq)

data Catalyst = Catalyst { id :: Int
                         , smiles :: Text
                         , name :: Maybe Text
                         }

data PRODUCT_FROM = PRODUCT_FROM { amount :: Float }

data ACCELERATE = ACCELERATE { temperature :: Float
                             , pressure :: Float
                             }

toReaction :: Monad m => Value -> m Reaction
toReaction v = do node <- exact v
                  let props = nodeProps node
                  let identity = nodeIdentity node
                  name <- (props `at` "name")  >>= exact
                  return $ Reaction identity name
