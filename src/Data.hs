{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data
       ( getReaction
       , addReaction
       )
       where

import Database.Bolt
import Data.Text as T (Text, pack)
import Data.Default
import Data.Maybe(fromJust)
import Data.Map as M (fromList, Map, empty, union)

data Molecule = Molecule { id :: Int
                         , smilesM :: Text
                         , iupacName :: Text
                         }

data Reaction = Reaction { id :: Int
                         , nameR :: Text
                         } deriving (Show, Eq)

data Catalyst = Catalyst { id :: Int
                         , smilesC :: Text
                         , nameC :: Maybe Text
                         }

data PRODUCT_FROM = PRODUCT_FROM { amount :: Double }

data ACCELERATE = ACCELERATE { temperature :: Double
                             , pressure :: Double
                             }

data FullReaction = FullReaction { firstIn :: Molecule
                                 , secondIn :: Molecule
                                 , out :: Molecule
                                 , catalyst :: Catalyst
                                 , reaction :: Reaction
                                 , productFrom :: PRODUCT_FROM
                                 , accelerate :: ACCELERATE
                                 }

runQuery :: BoltActionT IO a -> IO a
runQuery q = do pipe <- connect $ def { user = "neo4j", password = "neo4j" }
                run pipe q

toReaction :: Monad m => Value -> m Reaction
toReaction v = do node <- exact v
                  let props = nodeProps node
                  let identity = nodeIdentity node
                  name <- (props `at` "name")  >>= exact
                  return $ Reaction identity name

queryReaction :: Int -> BoltActionT IO Reaction
queryReaction ident = do result <- head <$> queryP cypher params
                         node <- result `at` "reaction"
                         toReaction node
  where cypher = "MATCH (reaction:Reaction) WHERE ID(reaction) = {ident} RETURN reaction"
        params = fromList [("ident", I ident)]

queryAddReaction :: FullReaction -> BoltActionT IO ()
queryAddReaction fr = queryP_ cypher params
  where cypher = T.pack $ "CREATE (r:Reaction { name: {nameR} })"
              ++ "CREATE UNIQUE (:Molecule{ smiles: {smiles1}, iupacName: {iupac1} })-[:REAGENT_IN]->(r)"
              ++ "CREATE UNIQUE (:Molecule{ smiles: {smiles2}, iupacName: {iupac2} })-[:REAGENT_IN]->(r)"
              ++ "CREATE UNIQUE (:Catalyst{smiles: {smilesC}, nameC: {nameC} })-[:ACCELERATE{temperature: {temp}, pressure: {pres}}]->(r)"
              ++ "CREATE UNIQUE (r)-[:PRODUCT_FROM{amount: {amount}}]->(:Molecule{smiles: {smilesO}, iupacName: {iupacO}})"
        params = fromList [ ("nameR", T $ nameR $ reaction fr)
                          , ("smiles1", T $ smilesM $ firstIn fr)
                          , ("iupac1", T $ iupacName $ firstIn fr)
                          , ("smiles2", T $ smilesM $ secondIn fr)
                          , ("iupac2", T $ iupacName $ secondIn fr)
                          , ("smilesC", T $ smilesC $ catalyst fr)
                          , ("nameC", T $ fromJust $ nameC $ catalyst fr)  -- maybe error, need fix
                          , ("temp", F $ temperature $ accelerate fr)
                          , ("pres", F $ pressure $ accelerate fr)
                          , ("amount", F $ amount $ productFrom fr)
                          , ("smilesO", T $ smilesM $ out fr)
                          , ("iupacO", T $ iupacName $ out fr)]

getReaction :: Int -> IO Reaction
getReaction ident = runQuery $ queryReaction ident

addReaction :: FullReaction -> IO ()
addReaction reaction = runQuery $ queryAddReaction reaction

shortestPath :: Int -> Int -> IO [Int]
shortestPath from to = shortestPathBFS to [from] [] M.empty

shortestPathBFS :: Int -> [Int] -> [Int] -> Map Int Int -> IO [Reaction]
shortestPathBFS to (to:q)
shortestPathBFS to [] _ prev = return []
shortestPathBFS to (q:queue) visited prev = do neighbours <- filter (`notElem` visited) <$> getProduced q
                                               let neighboursMap = fromList $ zip neighbours [q..]
                                               let newPrev = union neighboursMap prev
                                               shortestPathBFS to (queue ++ neighbours) (visited ++ neighbours) newPrev



getProduced :: Int -> IO [Int]
getProduced m = runQuery $ queryProduced m

queryProduced :: Int -> BoltActionT IO [Int]
queryProduced m = do result <- queryP cypher params
                     nodes <- traverse (`at` "prod") result
                     return $ map nodeIdentity nodes
 where cypher = T.pack $ "MATCH (m:Molecule)-[:REAGENT_IN]->(:Reaction)-[:PRODUCT_FROM]->(prod:Molecule)"
                      ++ "WHERE ID(m) = ident RETURN prod"
       params = fromList [("ident", I m)]