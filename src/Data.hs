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
import Data.Map as M (fromList, Map, empty, union, notMember, (!))
import Control.Monad((>=>))

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

queryReaction :: Text -> Map Text Value -> BoltActionT IO Reaction
queryReaction cypher params = do result <- head <$> queryP cypher params
                                 node <- result `at` "reaction"
                                 toReaction node


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
getReaction ident = runQuery $ queryReaction cypher params
   where cypher = "MATCH (reaction:Reaction) WHERE ID(reaction) = {ident} RETURN reaction"
         params = fromList [("ident", I ident)]

addReaction :: FullReaction -> IO ()
addReaction reaction = runQuery $ queryAddReaction reaction

shortestPath :: Int -> Int -> IO [Reaction]
shortestPath from to = shortestPathBFS to [from] [] M.empty

shortestPathBFS :: Int -> [Int] -> [Int] -> Map Int Int -> IO [Reaction]
shortestPathBFS to [] _ prev = return []
shortestPathBFS to (q:queue) visited prev | to == q = convertToReactions q prev
                                          | otherwise = do neighbours <- filter (`notElem` visited) <$> getProduced q
                                                           let neighboursMap = fromList $ zip neighbours [q..]
                                                           let newPrev = union neighboursMap prev
                                                           shortestPathBFS to (queue ++ neighbours) (visited ++ neighbours) newPrev

convertToReactions :: Int -> Map Int Int -> IO [Reaction]
convertToReactions v prev | notMember v prev = return []
                          | otherwise = pure (:) <*> getReactionBetween u v <*> convertToReactions u prev
                              where u = prev ! v

getReactionBetween :: Int -> Int -> IO Reaction
getReactionBetween from to = runQuery $ queryReaction cypher params
  where cypher = "MATCH (m1:Molecule)-[:REAGENT_IN]->(r:Reaction)-[:PRODUCT_FROM]->(m2:Molecule) WHERE ID(m1) = {idFrom} AND ID(m2) = {idTo} RETURN reaction"
        params = fromList [("idFrom", I from), ("idTo", I to)]


getProduced :: Int -> IO [Int]
getProduced m = runQuery $ queryProduced m

queryProduced :: Int -> BoltActionT IO [Int]
queryProduced m = do result <- queryP cypher params
                     nodes <- traverse ((`at` "prod") >=> exact) result
                     return $ map nodeIdentity nodes
 where cypher = T.pack $ "MATCH (m:Molecule)-[:REAGENT_IN]->(:Reaction)-[:PRODUCT_FROM]->(prod:Molecule)"
                      ++ "WHERE ID(m) = ident RETURN prod"
       params = fromList [("ident", I m)]