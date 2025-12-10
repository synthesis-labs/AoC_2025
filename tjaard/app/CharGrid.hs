module CharGrid 
  ( CharGrid
  , Coord(..)
  , fromLines
  , coords
  , at
  , neighbors
  , inBounds
  , insertMany
  ) where

import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map

-- | A coordinate with position and value
-- x = column (horizontal), y = row (vertical)
data Coord = Coord 
  { x   :: !Int
  , y   :: !Int
  , val :: !Char
  } deriving (Show, Eq, Ord)

-- | 2D character grid backed by Map for O(log n) lookups
data CharGrid = CharGrid 
  { gridMap :: !(Map.Map (Int, Int) Char)
  , width   :: !Int  -- number of columns
  , height  :: !Int  -- number of rows
  } deriving (Show)

-- | Parse lines of text into a CharGrid
-- First line = row 0, first character = column 0
fromLines :: [String] -> CharGrid
fromLines [] = CharGrid Map.empty 0 0
fromLines ls = CharGrid gmap w h
  where
    cells = [((x, y), c) | (y, row) <- zip [0..] ls
                         , (x, c)   <- zip [0..] row]
    gmap = Map.fromList cells
    h = length ls
    w = if null ls then 0 else length (head ls)

-- | Convert grid to list of coordinates
coords :: CharGrid -> [Coord]
coords g = [Coord x y v | ((x, y), v) <- Map.toList (gridMap g)]

-- | Look up character at position
at :: CharGrid -> (Int, Int) -> Maybe Char
at g pos = Map.lookup pos (gridMap g)

-- | Check if position is within grid bounds
inBounds :: CharGrid -> (Int, Int) -> Bool
inBounds g (x, y) = x >= 0 && y >= 0 && x < width g && y < height g

-- | Get all 8 surrounding coordinates
neighbors :: CharGrid -> Coord -> [Coord]
neighbors g (Coord x y _) = 
  mapMaybe toCoord [(x + dx, y + dy) | dx <- [-1..1]
                                      , dy <- [-1..1]
                                      , (dx, dy) /= (0, 0)
                                      , inBounds g (x + dx, y + dy)]
  where
    toCoord pos@(x', y') = Coord x' y' <$> at g pos

-- | Update values at multiple positions
insertMany :: [((Int, Int), Char)] -> CharGrid -> CharGrid
insertMany updates g = g { gridMap = foldr (uncurry Map.insert) (gridMap g) updates }

