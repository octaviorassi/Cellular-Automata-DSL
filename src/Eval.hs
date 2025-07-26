module Eval
  (
    eval,
    State
  )
where

import AST
import qualified Data.Vector as V

-- Example: 3x3 grid
grid :: Grid
grid = Grid (V.fromList [V.fromList [Dead, Alive, Dead],
                         V.fromList [Alive, Dead, Alive],
                         V.fromList [Dead, Alive, Dead]])

-- O(1) access by (row, col)
getCell :: Grid -> Int -> Int -> State
getCell (Grid g) row col = g V.! row V.! col

eval :: Exp a -> Position -> a
eval (Const n) _ = n
eval (UMinus ei) _ = negate (eval ei)
eval (Plus e1 e2) _ = (eval e1) + (eval e2)
eval ()

eval (Count es ei p) = undefined

{-
  Const           :: Int -> Exp Int
  Count           :: Exp State -> Exp Int
  UMinus          :: Exp Int -> Exp Int
  Plus            :: Exp Int -> Exp Int -> Exp Int
  Minus           :: Exp Int -> Exp Int -> Exp Int
  Times           :: Exp Int -> Exp Int -> Exp Int
  Div             :: Exp Int -> Exp Int -> Exp Int

  -- Expresiones booleanas
  BTrue           :: Exp Bool
  BFalse          :: Exp Bool
  IsNeighbor      :: Neighbor -> Exp State -> Exp Bool
  Lt              :: Exp Int -> Exp Int -> Exp Bool
  Gt              :: Exp Int -> Exp Int -> Exp Bool
  And             :: Exp Bool -> Exp Bool -> Exp Bool
  Or              :: Exp Bool -> Exp Bool -> Exp Bool
  Not             :: Exp Bool -> Exp Bool
  Eq              :: Exp Int -> Exp Int -> Exp Bool
  NEq             :: Exp Int -> Exp Int -> Exp Bool  

  -- Expresiones de estado
  EDead           :: Exp State
  EAlive          :: Exp State
  IfThenElse      :: Exp Bool -> Exp State -> Exp State -> Exp State
  NotState        :: Exp State -> Exp State
  WithProbability :: Probability -> Exp State -> Exp State -> Exp State

-}