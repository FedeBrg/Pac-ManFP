module PacParser where

import Data.Char (digitToInt)
import qualified Data.Map as Map
import Data.Ix (range)

import Types


--
-- 0 = 
--

--     |
-- 1 = |
--     |

--
-- 2 =  
--     _____

--     |
-- 3 = |
--     └────

--          |
-- 4 =      |
--          |

--     |    |
-- 5 = |    |
--     |    |

--          |
-- 6 =      |
--      ────┘

--     |   |
-- 7 = |   |
--     └───┘

--     ‾‾‾‾‾
-- 8 = 
--

--     ┌────
-- 9 = |
--     |

--     ‾‾‾‾‾
-- A = 
--     _____

--     ┌───  
-- B = |
--     └─── 

--     ────┐
-- C =     |
--         |

--     ┌────┐
-- D = |    |
--     |    |

--     ───┐
-- E =    |
--     ───┘

--     ┌───┐
-- F = |   |
--     └───┘



pacMap :: Pacmap
pacMap = Map.fromList (mapParser (range ((0,0),(18,20))))

stringMap :: [[Char]]
stringMap = 
  [ "9AAAAAAAA8AAAAAAAAC"
  , "59AA8AAAC59AAA8AAC5"
  , "55BE5BAE575BAE5BE55"
  , "51AA0A8A2A2A8A0AA45"
  , "55BE5D5BA8AE5D5BE55"
  , "53AA453AC59A651AA65"
  , "3AAC51AE575BA459AA6"
  , "AAE5559A2A2AC555BAA"
  , "BAA65759AAAC5753AAE"
  , "BAAA0A45BAE51A0AAAE"
  , "BAAC5D53AAA65D59AAE"
  , "AAE5551AAAAA4555BAA"
  , "9AA6575BA8AE5753AAC"
  , "59AA0A2AC59A2A0AAC5"
  , "55BC5BAE575BAE59E55"
  , "53C51A8A2A2A8A45965"
  , "1E575D5BA8AE5D575B4"
  , "592A653AC59A653A2C5"
  , "55BAA2AE575BA2AAE55"
  , "53AAAAAA2A2AAAAAA65"
  , "3AAAAAAAAAAAAAAAAA6"
  ]


mapParser :: [Location] -> [(Location, LocationLimits)]
mapParser locs = map (cellSpecToBounds) locs


cellSpecToBounds :: Location -> (Location, LocationLimits)
cellSpecToBounds loc@(x, y) =
  let c = ((stringMap!!(20-y))!!x)
      (topIsWall, rightIsWall, bottomIsWall, leftIsWall) = charToBoundsSet c
      topCell = if topIsWall then Wall else (Neighbour (x, y + 1))
      rightCell = if rightIsWall then Wall else (Neighbour (x + 1, y))
      bottomCell = if bottomIsWall then Wall else (Neighbour (x, y - 1))
      leftCell = if leftIsWall then Wall else (Neighbour (x - 1, y))
  in  (loc, LocationLimits topCell rightCell bottomCell leftCell)

numColumns :: Int
numColumns = 19

numRows :: Int
numRows = 21

charToBoundsSet :: Char -> (Bool, Bool, Bool, Bool)
charToBoundsSet c =
  ( num > 7
  , num `mod` 8 > 3
  , num `mod` 4 > 1
  , num `mod` 2 == 1
  )
  where
    num = digitToInt c


pointMap :: [Location]
pointMap = pointParser (range ((0,0),(18,20)))

pointString :: [[Char]]
pointString =  
  [ "0000000000000000000"
  , "0111111110111111110"
  , "0000100010100010000"
  , "0111111111111111110"
  , "0100101000001010010"
  , "0111101110111011110"
  , "0000100000000010000"
  , "0000100000000010000"
  , "0000100000000010000"
  , "0000100000000010000"
  , "0000100000000010000"
  , "0000100000000010000"
  , "0000100000000010000"
  , "0111111110111111110"
  , "0100100010100010010"
  , "0010111110111110100"
  , "0010101000001010100"
  , "0111101110111011110"
  , "0100000010100000010"
  , "0111111111111111110"
  , "0000000000000000000"
  ]



pointParser :: [Location] -> [Location]
pointParser locs = filter filterLoc locs


filterLoc :: Location -> Bool
filterLoc (x,y) = ((pointString!!(20-y))!!x) == '1'


