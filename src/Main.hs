module Main where


import qualified Data.Map as Map
import Data.Maybe (fromJust, catMaybes)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import PacParser(pacMap, pointMap)
import Types

import Control.Monad.Random
import Control.Monad.State (runState,State, get, put)

cellSize :: Float
cellSize = 25

xOffset :: Float
xOffset = -225

yOffset :: Float
yOffset = -250

windowDisplay :: Display
windowDisplay = InWindow "Window" (625, 525) (10, 10)

main :: IO ()
main = do
  gen <- getStdGen
  play
    windowDisplay
    black
    refreshRate
    (initWorld gen)
    drawWorld 
    inputHandler
    updateWorld

-- Function that draws the world, used in play
drawWorld :: World -> Picture
drawWorld world 
  | (worldResult world ) == GameLost =  Pictures [translate 0 50 (Scale 0.12 0.25 (Color white (Text "Game Over"))), (Scale 0.12 0.25 (Color white (Text "Score"))), translate 0 (-50) (Scale 0.12 0.25 (Color white (Text (show (score world)) )))]
  | (worldResult world ) == GameWon = Pictures [translate 0 50 (Scale 0.12 0.25 (Color white (Text "You won!"))),(Scale 0.12 0.25 (Color white (Text "Score"))), translate 0 (-50) (Scale 0.12 0.25 (Color white (Text (show (score world)) )))]
  |otherwise = Pictures (((pointPic 6.0) <$> powerUps world)++((pointPic 3.0) <$> points world) ++ (enemyPic <$> enemies world) ++ [mapGrid, playerMarker, translate 250 200 (Scale 0.12 0.25 (Color white (Text "Score"))), translate 250 150 (Scale 0.12 0.25 (Color white (Text (show (score world)) )))] )
  where
    playerPos = center (locationToCoords (playerLocation world))
    playerMarker = drawAt playerPos yellow (circleSolid 10)

    pointPic :: Float -> Location -> Picture
    pointPic size pointLoc = drawAt (center (locationToCoords pointLoc))  white (circleSolid size)

    enemyPic :: Enemy -> Picture
    enemyPic (Enemy loc color dead vulnerable dt m) = 
        let enemyPos = center (locationToCoords loc) in
        if vulnerable then drawAt enemyPos blue (circleSolid 10)
          else if dead then drawAt enemyPos color (Circle 10)
            else drawAt enemyPos color (circleSolid 10)

    drawAt :: (Float,Float) -> Color -> Picture -> Picture
    drawAt (x,y) color circ = translate x y (Color color circ)


    mapGrid = Pictures $ concatMap makeWallPictures (Map.toList (worldLimits world))

    makeWallPictures :: (Location, LocationLimits) -> [Picture]
    makeWallPictures ((x,y), LocationLimits up right down left) =
      let coords = locationToCoords (x,y)
          tl@(tlx, tly) = topLeft coords
          tr@(trx, try) = topRight coords
          bl@(blx, bly) = bottomLeft coords
          br@(brx, bry) = bottomRight coords
      in  catMaybes [ drawEdge (tr, tl, (tlx, tly - 2), (trx, try - 2)) up
          , drawEdge (br, tr, (trx-2, try), (brx-2, bry)) right
          , drawEdge (bl, br, (brx, bry+2), (blx, bly+2)) down
          , drawEdge (tl, bl, (blx+2, bly), (tlx+2, tly)) left
          ]

    drawEdge :: (Coordinates, Coordinates, Coordinates, Coordinates) -> LimitType -> Maybe Picture
    drawEdge (p1, p2, _, _) (Neighbour _) = Nothing
    drawEdge (p1, p2, p3, p4) _ = Just ( Color blue (Polygon [p1, p2, p3, p4]) )
-----------------------------------------------------------------------------------------------

-- Function that handles user input, used in play
inputHandler :: Event -> World -> World
inputHandler event w 
    | ((worldResult w) /= GameInProgress) = w
    | otherwise = case event of
      (EventKey (SpecialKey KeyUp) Down _ _) -> w {moved = True, bufferedMove = UpMove }
      (EventKey (SpecialKey KeyDown) Down _ _) -> w {moved = True, bufferedMove = DownMove }
      (EventKey (SpecialKey KeyRight) Down _ _) -> w {moved = True, bufferedMove = RightMove }
      (EventKey (SpecialKey KeyLeft) Down _ _) -> w {moved = True, bufferedMove = LeftMove }
      _ -> w
----------------------------------------------------------------------------------      

-- Function that updates the world, used in play
updateWorld :: Float -> World -> World
updateWorld _ w
  | (worldResult w) /= GameInProgress = w
  | enemyCollision (enemies w) (playerLocation w) = moveFunction (updateTimers (pickUps (handleCollision w)))
  | canTeleport (playerLocation w) (moved w) = updateTimers w { playerLocation = teleport (playerLocation w), moved = False}
  | otherwise = (moveFunction (pickUps (updateTimers w)))
-------------------

-- Helper to swap between Pacman and enemy movement
moveFunction :: World -> World
moveFunction w = if (pacTurn w) then movePacman w{pacTurn = False} else moveEnemies w{pacTurn = True}
---------------------------------------------------------------

-- Pacman Movement
movePacman :: World -> World
movePacman w = case (bufferedMove w) of
    (UpMove) -> w { playerLocation = nextLocation upLimit }
    (DownMove) -> w { playerLocation = nextLocation downLimit }
    (LeftMove) -> w { playerLocation = nextLocation leftLimit }
    (RightMove) -> w { playerLocation = nextLocation rightLimit }
  where
    cellBounds :: LocationLimits
    cellBounds = fromJust $ (Map.lookup (playerLocation w) (worldLimits w))

    nextLocation :: (LocationLimits -> LimitType) -> Location
    nextLocation limitFunc = case limitFunc cellBounds of
      (Neighbour nei) -> nei
      _ -> playerLocation w
-----------------------------------------------------------------------------------------

-- Handle Collisions with enemies and reacting accordingly
-- Check for a collision that needs attention
enemyCollision :: [Enemy] -> Location -> Bool
enemyCollision [] player = False
enemyCollision (e:es) player = if (((enemyLocation e) == player) && not (isDead e)) then True else enemyCollision es player

-- Handle collisions
handleCollision :: World -> World
handleCollision w 
  | (vulnerability w) = w {enemies = map (killEnemy (playerLocation w)) (enemies w), score = (score w) + 50} 
  | otherwise = w {worldResult = GameLost}  

----------------------------------------  


-- World functions to update states
updateTimers :: World -> World
updateTimers w = w {enemies = map (updateEnemy (vulnerability w)) (enemies w) , vulnerabilityTimer = updateTime (vulnerabilityTimer w), vulnerability = updateVulnerability (vulnerabilityTimer w)  }

updateVulnerability :: Int -> Bool
updateVulnerability 0 = False
updateVulnerability i = True
-------------------------------------------

-- Handle pick ups and scores
-- Wrapper for both things and checks winCondtion
pickUps :: World -> World
pickUps w = checkWin (pickPoint (pickPowerUp w))

-- Used to pick up points and increment score
pickPoint :: World -> World
pickPoint w =
  let newList = filter ((/=) (playerLocation w)) (points w) in
  if (length newList) == (length (points w)) then w else w {points = newList, score = (score w) + 1 }

-- Used to pick up PowerUps and triggering vulnerability
pickPowerUp :: World -> World
pickPowerUp w = 
  let newList = filter ((/=) (playerLocation w)) (powerUps w) in
  if (length newList) == (length (powerUps w)) then w else w {powerUps = newList, vulnerability = True, enemies = map makeVulnerable (enemies w), vulnerabilityTimer = (getSeconds 5), score = (score w) + 10}

-- Checks world's win condition
checkWin :: World -> World
checkWin w = if (length (points w) == 0 && length (powerUps w) == 0) then w{worldResult = GameWon} else w
---------------------------------------------------------------------------------------------------------------------------------------

-- Functions to handle enemy behaviour
-- Killing and enemy
killEnemy :: Location -> Enemy -> Enemy
killEnemy loc e = if loc == (enemyLocation e)  then e {isDead = True, deathTimer = (getSeconds 5), isVulnerable = False} else e

-- Check enemy death timer to revive it when needed
updateEnemy :: Bool -> Enemy -> Enemy
updateEnemy vul e = if (deathTimer e) == 0 then e{isDead = False, isVulnerable = vul} else e {deathTimer = updateTime (deathTimer e)}

-- Makes enemy vulnerable (used when PowerUps are picked up)
makeVulnerable :: Enemy -> Enemy
makeVulnerable e = if not (isDead e) then e {isVulnerable = True} else e

-- Makes enemy normal again after vulnerability
normalizeEnemy :: Enemy -> Enemy
normalizeEnemy e = if not (isDead e) then e {isVulnerable = False} else e
------------------------------------------------------------------------------



-- Teleport on map tunnels
teleport :: Location -> Location
teleport loc = if (loc == leftTP) then rightTP else leftTP 

leftTP :: Location
leftTP = (0,11)

rightTP :: Location
rightTP = (18,11)

canTeleport :: Location -> Bool -> Bool
canTeleport loc moved = if ((loc == leftTP || loc == rightTP) && moved) then True else False
------------------------------------------------------------------------------------------------

-- Enemy movement 
moveEnemies :: World -> World
moveEnemies w = w{enemies = newEnemies, randomGen = newGen}
  where 
    (newEnemies, newGen) = runState (sequence ((moveEnemy w)  <$> (enemies w))) (randomGen w)


moveEnemy :: World -> Enemy ->  State StdGen Enemy
moveEnemy w e = if (length posLocs == 0 || (enemyMoved e)) 
  then return e{enemyMoved = False} 
  else do 
    gen <- get
    let (randomIndex, newGen) = randomR (0, (length posLocs) - 1) gen
        newLoc = posLocs !! randomIndex
    put newGen
    return e {enemyLocation = newLoc, enemyMoved =True}
  where
    posLocs = posibleLocations (enemyLocation e) w

posibleLocations :: Location -> World -> [Location]
posibleLocations loc w = 
  let bounds = fromJust (Map.lookup loc (worldLimits w))
      maybeUpLoc = case upLimit bounds of
        (Neighbour loc) -> loc
        _ -> (0,0)
      maybeRightLoc = case rightLimit bounds of
        (Neighbour loc) -> loc
        _ -> (0,0)
      maybeDownLoc = case downLimit bounds of
        (Neighbour loc) -> loc
        _ -> (0,0)
      maybeLeftLoc = case leftLimit bounds of
        (Neighbour loc) -> loc
        _ -> (0,0)
      in filter ((/=) (0,0)) [maybeUpLoc, maybeRightLoc, maybeDownLoc, maybeLeftLoc]
----------------------------------------------------------------------------------------------------

-- Creates new world from scratch
initWorld :: StdGen -> World
initWorld gen = (World (9, 5) pacMap GameInProgress False UpMove [Enemy (7,13) red False False 0 True,Enemy (11,13) orange False False 0 True] False 0 0 pointMap [(1,5),(17,5),(1,18),(17,18)] gen True)
-------------------------------------------------------------------------------------------------------

-- Helper Functions

-- Get Screen actual X,Y given a discrete Location
locationToCoords :: Location -> ActualLocation
locationToCoords (x, y) = ActualLocation
  (centerX, centerY) -- Center
  (centerX - half, centerY + half) -- TL
  (centerX + half, centerY + half) -- TR
  (centerX + half, centerY - half) -- BR
  (centerX - half, centerY - half) -- BL

  where
    (centerX, centerY) = (xOffset + (fromIntegral x) * cellSize, yOffset + (fromIntegral y) * cellSize)
    half = cellSize / 2.0

-- Decrements timers, if 0 stays the same
updateTime :: Int -> Int
updateTime 0 = 0
updateTime i = i-1

-- Frames per second
refreshRate :: Int
refreshRate = 15

-- Takes seconds as imput and returns frames needed for that time
getSeconds :: Int -> Int
getSeconds s = s*refreshRate

