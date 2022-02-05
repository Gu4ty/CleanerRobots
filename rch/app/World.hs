module World(
    World, 
    Position, 
    Robot,
    Robots,
    initWorld, 
    getRobots, 
    getRobotsWithKids, 
    setRobots,
    setRobotsWithKids, 
    robotAt, 
    addRobotKid, 
    addRobot,
    removeRobot, 
    removeRobotKid, 
    getElements,
    getKids,
    addKid,
    initStatic,
    worldToString,
    blockAt,
    emptyAt,
    setKids,
    setBlocks,
    getEmptyCorral,
    getBlocks,
    getCorral,
    kidAt,
    addDirt,
    emptyAdjacent,
    bfs,
    dirtAt,
    corralAt, 
    removeDirtAt,
    removeKidAt,
    getDirt,
    getCleanPercent,
    isInteriorPoint) where

import Utils
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
-- import Debug.Trace

type Position = (Int, Int)
type Blocks = [Position]
type Dirts = [Position]
type Corral = [Position]
type Kids = [Position]
type RobotType = Int
type Robot = (Position, RobotType)
type Robots = [Robot]
type Elements =(Blocks, Dirts , Corral ,Kids, Robots, Robots) 
type Dimension = (Int, Int)
type World = (Elements, Dimension)

getElements:: World -> Elements
getElements (e,d) = e

getDimensions:: World -> Dimension
getDimensions (e,d) = d

getBlocks :: Elements -> [Position]
getBlocks (blocks,_,_,_,_,_) = blocks

setBlocks :: World -> [Position] -> World
setBlocks w b = ( (b,d,c,k, r, rk) , dimensions) where 
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    k = getKids (getElements w)
    r = getRobots (getElements w)
    rk = getRobotsWithKids (getElements w)
    dimensions = getDimensions w

getDirt :: Elements -> [Position]
getDirt (_,dirt,_,_,_, _) = dirt

getCorral :: Elements -> [Position]
getCorral (_,_,corral,_,_, _) = corral

getEmptyCorral:: World -> [Position]
getEmptyCorral w = getCorral(getElements w) \\ getKids(getElements w) 


getKids :: Elements -> [Position]
getKids (_,_,_,kids,_, _) = kids

setKids :: World -> [Position] -> World
setKids w k = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    r = getRobots (getElements w)
    rk = getRobotsWithKids (getElements w)
    dimensions = getDimensions w


getRobots :: Elements -> Robots
getRobots (_,_,_,_,robots, _) = robots

setRobots :: World -> [Robot] -> World
setRobots w r = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    k = getKids (getElements w)
    rk = getRobotsWithKids (getElements w)
    dimensions = getDimensions w


getRobotsWithKids :: Elements -> Robots
getRobotsWithKids (_,_,_,_,_,robots) = robots

setRobotsWithKids :: World -> [Robot] -> World
setRobotsWithKids w rk = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    k = getKids (getElements w)
    r = getRobots (getElements w)
    dimensions = getDimensions w


getRobotsPositions :: Robots -> [Position]
getRobotsPositions = map fst




initStatic :: Dimension -> World 
initStatic (n,m) = ( (
    [(6,3), (4,5)],  --block
    [(8,4), (2,5)],  --dirt
    [(0,0), (0,1), (1,0), (1,1)], --corral
    [(6,2)], --kid
    [((5,6),1), ((7,4), 2)],     --robots no kids
    []),    --robots kids     
    (n,m))

initWorld :: Int -> Int -> (Int,Int) -> Int -> Int -> Int -> (Int,Int) -> Int -> World
initWorld countBlocks dirtPercent (nc,mc) countKids countRR countRP (n,m) seed=
    let 
        c = makeCorral (nc,mc) (n,m) seed
        all = [(x,y) | x <- [0 .. n-1], y<- [0.. m-1], (x,y) `notElem` c]
        (b, rem) = selectItemsFromList countBlocks all seed
        (d, rem1) = selectItemsFromList (dirtPercent * n*m `div` 100) rem seed
        (k, rem2) = selectItemsFromList countKids rem1 seed
        (rrPos, rem3) = selectItemsFromList countRR rem2 seed
        rr = [ (pos, 1) | pos <- rrPos ]
        (rpPos, rem4) = selectItemsFromList countRP rem3 seed
        rp = [ (pos, 2) | pos <- rpPos ]
        r = merge rr rp
    in
        ( (b,d,c,k,r, []) , (n,m))


getCleanPercent:: World -> Float
getCleanPercent w = 
     100 - 100 * dirties/size where 
         dirtiesInt = length (getDirt (getElements w))
         sizeInt = uncurry (*) (getDimensions w)
         dirties = fromIntegral dirtiesInt 
         size = fromIntegral sizeInt 

makeCorral:: (Int, Int) -> (Int, Int) -> Int -> [Position]
makeCorral (cn,cm) (n,m) seed = 
    let 
        xInit = snd (randomInt(0, n- cn) seed)
        yInit = snd (randomInt(0, m - cm) seed)
    in 
        [(x,y)| x <- [xInit .. (xInit + cn -1)], y <- [yInit .. (yInit + cm -1)]]

whatsAt:: World -> Position -> String
whatsAt w pos = 
    let 
        wList = toList w
        markList = tupleToList ("b","d","c","k","rr", "rp", "rk")
        in
    "(" ++ 
    intercalate "," (filter (/= "") [markAt (positions,mark) pos | (positions,mark) <- zip wList markList]) 
    ++ ")"


markAt :: ([Position], String) -> Position -> String
markAt (positions,mark) position 
    |position `elem` positions = mark
    |otherwise =""


worldToString:: World -> String
worldToString w = 
    concat [rowString x | x <-[0 .. (n-1)]] where
    (n,m) = getDimensions w
    rowString x = intercalate "\t" [whatsAt w (x,y) | y<-[0 .. (m-1)]  ] ++ "\n"
    

toList:: World -> [[Position]]
toList w = tupleToList (b,d,c,k,rr,rp, rk) where
    b = getBlocks (getElements w)
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    k = getKids (getElements w)
    r = getRobots (getElements w)
    rr = getRobotsPositions(filter (\(_, t)-> t ==1) r)
    rp = getRobotsPositions(filter (\(_, t)-> t /=1) r)
    rk =getRobotsPositions ( getRobotsWithKids (getElements w))


blockAt:: World -> Position -> Bool 
blockAt w pos = pos  `elem` getBlocks(getElements w)

corralAt :: World -> Position -> Bool
corralAt w pos = pos  `elem` getCorral(getElements w)

kidAt:: World -> Position -> Bool
kidAt w pos = pos  `elem` getKids(getElements w)

dirtAt:: World -> Position -> Bool 
dirtAt w pos = pos `elem` getDirt(getElements w)

robotAt :: World -> Position -> Bool 
robotAt w pos = 
    pos `elem` map fst (getRobots(getElements w)) ||
    pos `elem` map fst (getRobotsWithKids (getElements w))


emptyAt:: World -> Position -> Bool
emptyAt w pos = whatsAt w pos =="()"

emptyAdjacent:: World -> Position -> [Position]
emptyAdjacent w (x0,y0) = [(x0 + x,y0 + y) | x <- [-1,0,1], y<- [-1,0,1],
                    isInteriorPoint w (x0 + x,y0 + y), emptyAt w (x0 + x,y0 + y) ] \\ [(x0,y0)]

isInteriorPoint:: World -> Position -> Bool
isInteriorPoint w (x,y) = (x < n) && (x >= 0) && (y < m) && (y >= 0) where
    (n,m) = getDimensions w 

addDirt:: World -> [Position] -> World
addDirt w dirt = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = merge (getDirt (getElements w)) dirt
    c = getCorral (getElements w)
    k = getKids (getElements w)
    r = getRobots (getElements w)
    rk = getRobotsWithKids (getElements w)
    dimensions = getDimensions w

addKid:: World -> Position -> World
addKid w kid = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    k = kid:getKids (getElements w)
    r = getRobots (getElements w)
    rk = getRobotsWithKids (getElements w)
    dimensions = getDimensions w

addRobotKid:: World -> Robot -> World
addRobotKid w robot = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    k = getKids (getElements w)
    r = getRobots (getElements w)
    rk = robot : getRobotsWithKids (getElements w)
    dimensions = getDimensions w

addRobot:: World -> Robot -> World
addRobot w robot = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    k = getKids (getElements w)
    r = robot : getRobots (getElements w)
    rk = getRobotsWithKids (getElements w)
    dimensions = getDimensions w

removeDirtAt:: World -> Position -> World
removeDirtAt w pos = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = getDirt (getElements w) \\ [pos]
    c = getCorral (getElements w)
    k = getKids (getElements w)
    r = getRobots (getElements w)
    rk = getRobotsWithKids (getElements w)
    dimensions = getDimensions w

removeKidAt :: World -> Position -> World
removeKidAt w pos = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    k = getKids (getElements w) \\ [pos]
    r = getRobots (getElements w)
    rk = getRobotsWithKids (getElements w)
    dimensions = getDimensions w

removeRobot :: World -> Robot -> World 
removeRobot w robot = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    k = getKids (getElements w) 
    r = getRobots (getElements w) \\ [robot]
    rk = getRobotsWithKids (getElements w)
    dimensions = getDimensions w

removeRobotKid :: World -> Robot -> World 
removeRobotKid w robot = ( (b,d,c,k,r, rk) , dimensions) where 
    b = getBlocks (getElements w)
    d = getDirt (getElements w)
    c = getCorral (getElements w)
    k = getKids (getElements w) 
    r = getRobots (getElements w) 
    rk = getRobotsWithKids (getElements w) \\ [robot]
    dimensions = getDimensions w

bfs:: World -> (World -> Position -> [Position]) -> [Position] -> [Position]
    ->(Map Position Int, Map Position Position)
bfs w adjFunc source destination = _bfs w adjFunc source destination source initDistances Map.empty  where
    initDistances = foldr (`Map.insert` 0) Map.empty source

_bfs:: World -> (World -> Position -> [Position]) -> [Position] -> [Position] ->[Position] 
   ->Map Position Int -> Map Position Position-> (Map Position Int, Map Position Position)
_bfs w adjFunc [] dest visited distances parents = (distances, parents)
_bfs w adjFunc source [] visited distances parents = (distances, parents)
_bfs w adjFunc (s: ss) dest visited  distances parents = 
    _bfs w adjFunc newSource newDest newVisited newDistances newParents where 
        adjPos = [position | position <- adjFunc w s, position `notElem` visited]
        newSource = merge ss adjPos
        newDest = dest \\ [s]
        newVisited = merge visited adjPos
        newDistances = foldr (\a map -> Map.insert a (map Map.! s + 1) map) distances adjPos
        newParents = foldr (`Map.insert` s) parents adjPos


