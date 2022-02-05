module Kids (allKidsMove,getFreeKids) where

import World
import Utils
import Data.List
-- import Debug.Trace

allKidsMove:: World -> Int -> Bool -> World
allKidsMove w  = moveFreeKids w freeKids where
    freeKids = getFreeKids w

moveFreeKids:: World -> [Position] -> Int -> Bool -> World
moveFreeKids w [] s d= w
moveFreeKids w (k: kss) s isNewDirt = moveFreeKids newW kss s isNewDirt where
    afterKidMove = kidMove w k s 
    newW = generateDirt afterKidMove k s isNewDirt

generateDirt:: World -> Position -> Int -> Bool -> World 
generateDirt w pos seed False = w
generateDirt w pos seed True = addDirt w (fst (selectItemsFromList (howManyDirt w pos seed) (emptyAdjacent w pos) seed))

howManyDirt:: World -> Position ->Int -> Int 
howManyDirt w pos seed = snd (randomInt (0, maxDirt) seed)  where 
   maxDirt = min (length (emptyAdjacent w pos) )  (dirtCountByNumberOfKids (countCloseKids w pos))

dirtCountByNumberOfKids:: Int -> Int
dirtCountByNumberOfKids 1 = 1
dirtCountByNumberOfKids 2 = 3
dirtCountByNumberOfKids c = 3

countCloseKids :: World -> Position -> Int
countCloseKids w (x0,y0) = length [(x0 + x,y0 + y) | x <- [-1,0,1], y<- [-1,0,1],
                    isInteriorPoint w (x0 + x,y0 + y), kidAt w (x0 + x,y0 + y) ]

kidMove :: World -> Position -> Int -> World
kidMove w pos seed = makeMove w pos (pickOne (filter (valid w pos) (moves w pos)) seed )

makeMove:: World -> Position -> Position -> World
makeMove w pos0 pos1
    |blockAt w pos1 = makeBlockMove w pos0 pos1 
    |pos0 == pos1 = w
    |otherwise = makeSimpleMove w pos0 pos1 

makeSimpleMove :: World-> Position -> Position -> World
makeSimpleMove w pos0 pos1= setKids w newKids where
    kids = getKids(getElements w)
    kidsDelete = kids \\ [pos0]
    newKids = pos1 : kidsDelete

makeBlockMove:: World -> Position -> Position -> World
makeBlockMove w (x0,y0) (x1,y1) = makeSimpleMove (setBlocks w newBlocks) (x0,y0) (x1,y1) where
    blocks = getBlocks (getElements w)
    empty = firstNonBlockPos w (x1,y1) (x1 - x0, y1- y0)
    blockDelete = blocks \\ [(x1,y1)] 
    newBlocks = empty : blockDelete

moves:: World -> Position -> [Position]
moves w (x0,y0) = [(x0 + x,y0 + y)| x <- [-1,0,1], y<- [-1,0,1],
                    emptyAt w (x0 + x,y0 + y) || blockAt w (x0 + x,y0 + y),
                    isInteriorPoint w (x0 + x,y0 + y)] ++ [(x0,y0)]
    
valid:: World ->Position -> Position -> Bool
valid w pos0 pos
    |blockAt w pos = validBlock w pos0 pos
    |pos == pos0 = True
    |otherwise = True

validBlock:: World -> Position -> Position -> Bool
validBlock w (x0,y0) (x1,y1) = emptyAt w (x,y) && isInteriorPoint w (x,y) where
    (x,y) = firstNonBlockPos w (x1,y1) (x1 - x0, y1 - y0) 

firstNonBlockPos:: World -> Position -> (Int, Int) -> Position
firstNonBlockPos w (x,y) (vx, vy)
    |blockAt w (x, y) = firstNonBlockPos w (x+vx, y + vy) (vx,vy)
    |otherwise = (x,y)

getFreeKids:: World -> [Position]
getFreeKids w = getKids (getElements w)  \\  getCorral(getElements w)