{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database
import Data.Text

main :: IO ()
main = do reactions <- shortestPath 23 78
          let r = Reaction 0 "ololo"
              m1 = Molecule 0 "22" "33"
              m2 = Molecule 0 "11" "t"
              m3 = Molecule 0 "7" "t"
              c = Catalyst 0 "2" $ Just "t"
              ac = ACCELERATE 3.9 22.4
              reac = PRODUCT_FROM 5.5
              fr = FullReaction m1 m2 m3 c r reac ac
          addReaction fr
          print reactions
