module Main where

import Utils
import Config
import World
import Kids
import Robots
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

type Stats = ((Int,Int), [Float])

simulate:: World -> Int -> Int -> Int -> (World, Stats)
simulate w maxTime  varTime seed = _simulate w maxTime varTime seed 0 ((0,maxTime), [])

_simulate:: World -> Int -> Int -> Int -> Int -> Stats -> (World, Stats)
_simulate w maxTime  varTime seed currentTime ((_,_), cleanPercents) = 
  if (currentTime == maxTime) || (getCleanPercent w  < 60)
    then
      trace(show(getCleanPercent w)) (w,((currentTime,maxTime), cleanPercents)) 
    else 
      let 
        w1 = allRobotsMove w
        w2 =allKidsMove w1 seed (currentTime /=0  && (currentTime `mod` varTime ==0)) 
      in 
        _simulate w2 maxTime varTime (newSeedFromSeed seed) (currentTime + 1) 
           ((currentTime +1, maxTime), merge cleanPercents [getCleanPercent w ]  ) 


resume:: World -> Stats -> String
resume w ((time, maxTime), cleanPercents) = 
 "Simulation with max time " ++ show maxTime ++ " ended in time: " ++ show time ++ " \n" ++
 "Simulation ended with a clean percent of " ++ show(last cleanPercents) ++ " \n" ++
 "In average, there was a " ++ show( sum cleanPercents /fromIntegral(length cleanPercents)) ++ "% of clean cells \n" ++
 "Final state of the grid is \n " ++ worldToString w


main :: IO ()
main = do
  let 
    world =  initWorld blockCount dirtPercent corralDimensions 
      kidCount reactiveRobotCount proactiveRobotCount worldDimension seed
    (w, stats) =simulate world maxTime variationTime seed
    in
    -- putStr ("Final state: \n" ++ worldToString  w )
    putStr (resume w stats)
