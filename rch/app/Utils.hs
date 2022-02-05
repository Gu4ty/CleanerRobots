module Utils
  ( randomInt,
    newSeedFromSeed, 
    selectItemsFromList,
    tupleToList,
    merge,
    pickOne,
    pushBack
  ) where

import Control.Monad.State (State, evalState, get, put)
import Data.Fixed          (mod')
import System.Random       (Random, StdGen, mkStdGen, random,randomIO, randomR)
import Data.List

-- ***************************************** Utils random *****************************************
type R a = State StdGen a

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

rand :: R Int
rand = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r

rRand :: Int -> Int
rRand seed = do
  let r = runRandom rand seed
  if r == seed
    then runRandom rand (seed + 1)
    else r

rRandom :: Int -> Int -> (Int, Int)
rRandom seed top =
  let nextSeed = rRand seed
   in (nextSeed, mod nextSeed top)

randomInt :: (Int, Int) -> Int -> (Int, Int)
randomInt (a, b) seed
  | b > a =
    let (newSeed, number) = rRandom seed (b - a + 1)
     in (newSeed, number + a)
  | otherwise = (seed, a)

newSeedFromSeed :: Int  -> Int
newSeedFromSeed seed = fst (randomInt (0,5) seed)

-- **********************************************************************************

selectItemsFromList::(Eq a) => Int -> [a]-> Int -> ([a], [a])
selectItemsFromList n l seed = (selected, remaining) where
  selected = pickN n l [] seed 
  remaining = l \\ selected

pickN ::(Eq a) => Int -> [a] -> [a] -> Int -> [a]-- asume all elements in list are distintc
pickN 0 l current seed = current 
pickN n [] current seed = []
pickN n l current seed = pickN (n-1) newL newCurrent newSeed where
  size = length l 
  ( newSeed, index) = randomInt (0, size -1) seed
  pick = l !! index
  newCurrent = pick : current
  newL = [x | x<- l,  x /= pick] 


pickOne::(Eq a) => [a]-> Int -> a
pickOne l seed = head selected where 
  (selected,_) = selectItemsFromList 1 l seed

tupleToList:: (a,a,a,a,a, a, a) -> [a]
tupleToList (a,b,c,d,e, f, g) =[a,b,c,d,e, f, g] 

merge:: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) ys = x : merge xs ys

pushBack:: [a]-> a -> [a]
pushBack xs a = foldr (:) [a] xs