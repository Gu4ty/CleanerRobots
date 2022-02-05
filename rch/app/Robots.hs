module Robots (getPath,exploreFunction,getMinimun,moveRobotWithoutKids, allRobotsMove) where

import World
import Kids
import Utils
import Data.List
import Data.Function 
import Data.Map (Map)
import qualified Data.Map as Map

-- import Debug.Trace

allRobotsMove:: World ->  World
allRobotsMove w  = 
    let 
        (w1, targets) = allRobotWithoutKidMove w (getRobots (getElements w)) []
        w2 = allRobotWithKidMove w1 (getRobotsWithKids (getElements w)) targets
    in 
        w2

allRobotWithoutKidMove:: World -> Robots -> [Position] -> (World, [Position])
allRobotWithoutKidMove w [] targets = (w, targets)
allRobotWithoutKidMove w (x: xss) targets = allRobotWithoutKidMove newW xss newTargets  where
    (newW, newTargets) = moveRobotWithoutKids w x targets

allRobotWithKidMove:: World -> Robots -> [Position] -> World 
allRobotWithKidMove w [] targets = w
allRobotWithKidMove w (x: xss) targets = allRobotWithKidMove newW xss newTargets  where
    (newW, newTargets) = moveRobotWithKid w x targets


moveRobotWithKid :: World -> Robot -> [Position] -> (World, [Position])
moveRobotWithKid w (pos, robotType) targetedPos 
    |robotType ==1 && dirtAt w pos = (removeDirtAt w pos, targetedPos)
    |robotType ==1 && corralAt w pos = _moveRobotWithKid w (pos, robotType) pos (getCorralTarget w) targetedPos
    |robotType ==1 = _moveRobotWithKid w (pos, robotType) pos (merge corral dirt) targetedPos
    |corralAt w pos = _moveRobotWithKid  w (pos, robotType) pos (getCorralTarget w) targetedPos
    |otherwise = _moveRobotWithKid w (pos, robotType) pos corral targetedPos
    where 
        corral = filter (`notElem` targetedPos) (getEmptyCorral w)
        dirt   = filter (`notElem` targetedPos) (getDirt (getElements w)) 


_moveRobotWithKid :: World -> Robot -> Position -> [Position]  -> [Position] -> (World, [Position])
_moveRobotWithKid w (p,t) pos targets targeted = (newW, newTargeted) where
    (targetedPos, path) = findClosest (bfs w exploreFunction [pos] targets) targets
    newTargeted1 = if targetedPos /= (-1,-1) then merge targeted [targetedPos] else targeted
    newW1 = makeTwoStep w (p,t) pos path
    dirt =filter (`notElem` targeted) (getDirt (getElements w)) 
    (newW, newTargeted) = if targetedPos == (-1,-1) && t /=1 then 
        _moveRobotWithKid w (p, t) pos dirt targeted 
    else 
        (newW1, newTargeted1)

makeTwoStep:: World -> Robot -> Position -> [Position] -> World
makeTwoStep w robot pos [] = w
makeTwoStep w (robPos,robType) pos [x]
    |[x] == getCorralTarget w = dropKid w (robPos,robType)
    |otherwise = setRobotsWithKids w newRobots where 
        robots = getRobotsWithKids (getElements w)
        robotsDelete = robots \\ [(robPos,robType)]
        newRobots = (x, robType) : robotsDelete
makeTwoStep w (robPos, robType) pos (x:(y:ss)) =
    makeTwoStep w (robPos, robType) pos [y] 

dropKid:: World -> Robot -> World
dropKid w (p,t) = 
    let 
        w1 = removeRobotKid w (p,t)
        w2 = addKid w1  p
        w3 = addRobot w2 (p,t)
    in 
        w3

getCorralTarget :: World -> [Position]
getCorralTarget w = 
    let 
        corral = getCorral (getElements w)
    in 
        [minimum corral]
        -- [minimumBy (compare `on` (length . emptyAdjacent w)) corral]


moveRobotWithoutKids:: World -> Robot -> [Position] -> (World, [Position])
moveRobotWithoutKids w (pos, robotType) targetedPos  
    |robotType ==1 && dirtAt w pos = (removeDirtAt w pos, targetedPos)
    |robotType == 1 = _moveRobotWithoutKids w (pos, robotType) pos (merge kids dirts) targetedPos
    |otherwise = _moveRobotWithoutKids  w (pos, robotType) pos kids targetedPos
    where 
        kids = filter (`notElem` targetedPos) (getFreeKids w)
        dirts = filter (`notElem` targetedPos) (getDirt (getElements w))


_moveRobotWithoutKids :: World -> Robot -> Position -> [Position]  -> [Position] -> (World, [Position])
_moveRobotWithoutKids w robot pos targets targeted = (newW, newTargeted) where
    (targetedPos, path) = findClosest (bfs w exploreFunction [pos] targets) targets
    newTargeted = if targetedPos /= (-1,-1) then merge targeted [targetedPos] else targeted
    newW = makeOneStep w robot pos path



makeOneStep:: World -> Robot -> Position -> [Position] -> World
makeOneStep w robot pos [] = w
makeOneStep w (robPos, robType) pos (x: xss)
    |kidAt w x = makeNewRobotWithKid w (robPos, robType) pos x
    |otherwise = setRobots w newRobots where 
        robots = getRobots (getElements w) 
        robotsDelete = robots \\ [(robPos, robType)]
        newRobots = ( x, robType) : robotsDelete

makeNewRobotWithKid:: World -> Robot -> Position -> Position -> World
makeNewRobotWithKid w (robPos, robType) pos x =
    addRobotKid (removeRobot (removeKidAt w x) (robPos, robType)) (x, robType)


findClosest:: (Map Position Int, Map Position Position) ->[Position] -> (Position, [Position])
findClosest (distances,paths) targets = (closestPos, path) where 
    closestPos = getMinimun (Map.toList distances) targets
    path = getPath paths closestPos

getMinimun:: [(Position, Int)] -> [Position] -> Position
getMinimun distances targets = _getMinimun (filter (\(p,d) -> p `elem` targets) distances ) (-1,-1) 10000000

_getMinimun:: [(Position, Int)] -> Position -> Int -> Position
_getMinimun [] bestPos bestDist = bestPos
_getMinimun ((pos,d) :xss) bestPos bestDist = 
    if d < bestDist then _getMinimun xss pos d else _getMinimun xss bestPos bestDist

getPath:: Map Position Position -> Position -> [Position]
getPath parents pos = _getPath parents pos []

_getPath:: Map Position Position  -> Position -> [Position] -> [Position]
_getPath parents pos path 
    |Map.member pos parents = _getPath parents (parents Map.! pos) (pos:path )
    |otherwise = path 


exploreFunction:: World -> Position -> [Position]
exploreFunction w (x0,y0) 
    |kidAt w (x0,y0) && not (corralAt w (x0,y0)) = []
    |otherwise = [(x0 + x,y0 + y) | x <- [-1,0,1], y<- [-1,0,1],
                    isInteriorPoint w (x0 + x,y0 + y),
                    not (blockAt w (x0 +x, y0 + y)),
                    not (robotAt w (x0 +x, y0 + y)),
                    not (kidAt w (x0 + x, y0 + y)) || not (corralAt w (x0 + x, y0 + y))] \\ [(x0,y0)]