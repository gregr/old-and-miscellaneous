module Board where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)

adjacentCoords boardSize (x, y) = xContrib ++ yContrib
    where xContrib = if x == 0
                     then [(x+1, y)]
                     else if x == boardSize
                          then [(x-1, y)]
                          else [(x+1, y), (x-1, y)]
          yContrib = if y == 0
                     then [(x, y+1)]
                     else if y == boardSize
                          then [(x, y-1)]
                          else [(x, y+1), (x, y-1)]


data Color = Black | White deriving (Enum, Eq, Ord, Show)

nextColor Black = White
nextColor _ = Black


--color, occupied coords, liberty coords
data Chain = Chain { color :: Color,
                     occupied, liberties :: Set Coord }
             deriving (Show)

newChain color coord libs = Chain color (S.fromList [coord]) $ S.fromList libs

takeLiberty chain libCoord =
    chain{liberties=(S.delete libCoord (liberties chain))}

extendChain chain coord newLibs = chain{occupied=occ, liberties=libs}
    where occ = S.insert coord $ occupied chain
          libs = S.union (S.fromList newLibs) $ liberties chain

mergeChains c1 c2 = c1{occupied=occ, liberties=libs}
    where occ = S.union (occupied c1) $ occupied c2
          libs = S.union (liberties c1) $ liberties c2

--merged index coord to chain
type ChainMap = Map Coord Chain

--index coord to merged index coord
type CoordMap = Map Coord Coord

--second Coord is the Chain index
data Board = Board {size :: Int, coords :: CoordMap}
             deriving (Show)

data Position = Position {colorToPlay :: Color, board :: Board,
                          indices :: CoordMap, chains :: ChainMap}
                deriving (Show)

isWithinBounds size (x, y) = (x >= 0 && x <= size) && (y >= 0 && y <= size)

isEmpty coords coord = notMember coord coords

isLegalMove board coord =
    (isWithinBounds (size board) coord) && (isEmpty (coords board) coord)

--check for legal move first?
--move (Position color (Board size coordToIndex) indexToId idToChain) coord =
--    Position (nextColor color) newBoard newIds newChains
--        where newBoard =
