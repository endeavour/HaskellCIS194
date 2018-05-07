{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Data.List
import Control.Monad
import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
 let attackingUnits = min ((attackers bf) - 1) 3
     defendingUnits = min (defenders bf) 2
 attackerRolls <- (reverse . sort) <$> replicateM attackingUnits die
 defenderRolls <- (reverse . sort) <$> replicateM defendingUnits die
 let battleResults = zipWith (>) attackerRolls defenderRolls
     attackerWins = length $ filter id battleResults
     defenderWins = length $ filter not battleResults

 return Battlefield { attackers = attackers bf - defenderWins, defenders = defenders bf - attackerWins}

isEndOfMatch :: Battlefield -> Bool
isEndOfMatch bf = attackers bf <= 1 || defenders bf <= 0

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
  newBf <- battle bf
  if isEndOfMatch newBf then return newBf else invade newBf

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  let runs = 1000
  results <- replicateM runs (invade bf)
  let defendersDestroyed = fromIntegral $ length $ filter (\endBf -> defenders endBf <= 0) results
  return (fromIntegral defendersDestroyed / fromIntegral runs)

