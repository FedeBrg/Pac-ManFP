module Types where

import qualified Data.Map as Map

import Graphics.Gloss.Interface.IO.Interact
import Control.Monad.Random



type Location = (Int, Int)

type Coordinates = (Float, Float)

data ActualLocation = ActualLocation
  { center :: Coordinates
  , topLeft :: Coordinates
  , topRight :: Coordinates
  , bottomRight :: Coordinates
  , bottomLeft :: Coordinates
  }

data LimitType =  Wall | Neighbour Location

data LocationLimits = LocationLimits
  { upLimit :: LimitType
  , rightLimit :: LimitType
  , downLimit :: LimitType
  , leftLimit :: LimitType
  }


type Pacmap = Map.Map Location LocationLimits


data GameResult = GameInProgress | GameWon | GameLost deriving (Eq)

data World = World
  { playerLocation :: Location
  , worldLimits :: Pacmap
  , worldResult :: GameResult
  , moved :: Bool
  , bufferedMove :: MoveDirection
  , enemies :: [Enemy]
  , vulnerability :: Bool
  , vulnerabilityTimer :: Int
  , score :: Int
  , points :: [Location]
  , powerUps :: [Location]
  , randomGen :: StdGen
  , pacTurn :: Bool
  }

data MoveDirection = UpMove | DownMove | LeftMove | RightMove

data Enemy = Enemy 
  { enemyLocation :: Location
  , enemyColor :: Color
  , isDead :: Bool
  , isVulnerable :: Bool
  , deathTimer :: Int
  , enemyMoved :: Bool
  }



