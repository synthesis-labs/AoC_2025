module Util.GridUtils.DirectionVonNeumann where

import Util.GridUtils.Coord (Coord (Coord))

data Direction
  = N
  | E
  | S
  | W
  deriving (Show, Eq, Enum)

turnRight :: Direction -> Direction
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

applyDirection :: Direction -> Coord -> Coord
applyDirection N (Coord x y) = Coord (x - 1) y
applyDirection S (Coord x y) = Coord (x + 1) y
applyDirection W (Coord x y) = Coord x (y - 1)
applyDirection E (Coord x y) = Coord x (y + 1)
