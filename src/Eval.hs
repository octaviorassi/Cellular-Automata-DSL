module Eval
  (
    eval,
    State
  )
where

import qualified Data.Vector as V

data Cell = Dead | Alive
newtype Grid = Grid (V.Vector (V.Vector Cell))  -- 2D grid
type State = Grid
type Step = State -> State



-- Example: 3x3 grid
grid :: Grid
grid = Grid 3 3 (V.fromList [
  V.fromList [Dead, Alive, Dead],
  V.fromList [Alive, Dead, Alive],
  V.fromList [Dead, Alive, Dead]
])

-- O(1) access by (row, col)
getCell :: Grid -> Int -> Int -> Cell
getCell g row col = g V.! row V.! col