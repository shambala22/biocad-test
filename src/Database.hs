{-# LANGUAGE OverloadedStrings #-}
module Database
       ( getReaction
       , addReaction
       , shortestPath
       , Reaction(..)
       , Catalyst(..)
       , Molecule(..)
       , ACCELERATE(..)
       , PRODUCT_FROM(..)
       , FullReaction(..)
       ) where

import           Database.Bolt
import           Data.Default   (def)
import           Data.Map as M  (Map, empty, fromList, union, notMember, (!), insert)
import           Data.Text as T (Text, pack)
import           Data.Maybe     (fromMaybe)
import           Control.Monad  ((>=>))
import           Data.List      (nub)

import           Data

getReaction :: Int -> IO Reaction
getReaction ident = runQuery $ queryReaction cypher params
   where cypher = "MATCH (reaction:Reaction) WHERE ID(reaction) = {ident} RETURN reaction"
         params = fromList [("ident", I ident)]

addReaction :: FullReaction -> IO ()
addReaction reaction = runQuery $ queryAddReaction reaction

shortestPath :: Int -> Int -> IO [Reaction]
shortestPath from to = shortestPathBFS to [from] [] M.empty


runQuery :: BoltActionT IO a -> IO a
runQuery q = do pipe <- connect $ def { user = "neo4j", password = "neo4j" }
                run pipe q

queryReaction :: Text -> Map Text Value -> BoltActionT IO Reaction
queryReaction cypher params = do result <- head <$> queryP cypher params
                                 node <- result `at` "reaction"
                                 toReaction node

queryAddReaction :: FullReaction -> BoltActionT IO ()
queryAddReaction fr = do ident <- queryCreateReaction $ reaction fr
                         queryP_ cypher $ insert "ident" (I ident) params
  where cypher = T.pack $ "MATCH (r:Reaction) WHERE ID(r) = {ident}"
              ++ "MERGE (m1:Molecule{ smiles: {smiles1}, iupacName: {iupac1} }) "
              ++ "MERGE (m2:Molecule{ smiles: {smiles2}, iupacName: {iupac2} }) "
              ++ "MERGE (c:Catalyst{ smiles: {smilesC}, name: {nameC} }) "
              ++ "MERGE (m3:Molecule{smiles: {smilesO}, iupacName: {iupacO}}) "
              ++ "MERGE (m1)-[:REAGENT_IN]->(r)-[:PRODUCT_FROM {amount: {amount}}]->(m3) "
              ++ "MERGE (m2)-[:REAGENT_IN]->(r) "
              ++ "MERGE (c)-[:ACCELERATE{temperature: {temp}, pressure: {pres}}]->(r)"
        params = fromList [ ("smiles1", T $ smilesM $ firstIn fr)
                          , ("iupac1", T $ iupacName $ firstIn fr)
                          , ("smiles2", T $ smilesM $ secondIn fr)
                          , ("iupac2", T $ iupacName $ secondIn fr)
                          , ("smilesC", T $ smilesC $ catalyst fr)
                          , ("nameC", T $ fromMaybe "" $ nameC $ catalyst fr)
                          , ("temp", F $ temperature $ accelerate fr)
                          , ("pres", F $ pressure $ accelerate fr)
                          , ("amount", F $ amount $ productFrom fr)
                          , ("smilesO", T $ smilesM $ out fr)
                          , ("iupacO", T $ iupacName $ out fr)]

queryCreateReaction :: Reaction -> BoltActionT IO Int
queryCreateReaction r = do record <- head <$> queryP cypher params
                           node <- record `at` "r" >>= exact
                           return $ nodeIdentity node
  where cypher = "CREATE (r:Reaction { name: {nameR}}) RETURN r"
        params = fromList [("nameR", T $ nameR r)]

shortestPathBFS :: Int -> [Int] -> [Int] -> Map Int Int -> IO [Reaction]
shortestPathBFS to [] _ prev = return []
shortestPathBFS to (q:queue) visited prev | to == q = reverse <$> convertToReactions q prev
                                          | otherwise = do neighbours <- nub <$> filter (`notElem` visited) <$> getProduced q
                                                           let neighboursMap = fromList $ zip neighbours $ repeat q
                                                           let newPrev = union neighboursMap prev
                                                           shortestPathBFS to (queue ++ neighbours) (visited ++ neighbours) newPrev

convertToReactions :: Int -> Map Int Int -> IO [Reaction]
convertToReactions v prev | notMember v prev = return []
                          | otherwise = pure (:) <*> getReactionBetween u v <*> convertToReactions u prev
                              where u = prev ! v

getReactionBetween :: Int -> Int -> IO Reaction
getReactionBetween from to = runQuery $ queryReaction cypher params
  where cypher = "MATCH (m1:Molecule)-[:REAGENT_IN]->(reaction:Reaction)-[:PRODUCT_FROM]->(m2:Molecule) WHERE ID(m1) = {idFrom} AND ID(m2) = {idTo} RETURN reaction"
        params = fromList [("idFrom", I from), ("idTo", I to)]


getProduced :: Int -> IO [Int]
getProduced m = runQuery $ queryProduced m

queryProduced :: Int -> BoltActionT IO [Int]
queryProduced m = do result <- queryP cypher params
                     nodes <- traverse ((`at` "prod") >=> exact) result
                     return $ map nodeIdentity nodes
 where cypher = T.pack $ "MATCH (m:Molecule)-[:REAGENT_IN]->(:Reaction)-[:PRODUCT_FROM]->(prod:Molecule)"
                      ++ "WHERE ID(m) = {ident} RETURN prod"
       params = fromList [("ident", I m)]