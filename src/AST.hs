{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST
  ( Program(..)
  , State(..)
  , Neighborhood(..)
  , Probability(..)
  , Neighbor(..)
  , Exp(..)
  , Position
  , Grid
  , GridSize
  , Layout
  , Step
  , Seed
  -- ... other exports
  ) where

import qualified Data.Vector as V

data Neighborhood = Moore | VonNeumann deriving (Show)
data Neighbor     =   TopLeft     | Top    | TopRight  
                    | LeftNeigh   | Self   | RightNeigh  
                    | BottomLeft  | Bottom | BottomRight 
                    deriving (Show, Eq)

type GridSize = (Int, Int)
type Grid = (V.Vector (V.Vector State))
type Position = (Int, Int) 
type Layout = [Position]
data Probability = Prob Double | Random deriving (Show, Eq)
data State = Dead | Alive deriving (Show, Eq)

type Step = Exp State
type Seed = Int
data Program = Program GridSize Neighborhood Step Layout Seed | InvalidProgram deriving (Show)

data Exp a where
  
  -- Expresiones enteras
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

deriving instance Show (Exp a)
deriving instance Eq (Exp a)


defaultGridSize :: (Int, Int)
defaultGridSize = (25, 25)  

defaultNeighborhood :: Neighborhood
defaultNeighborhood = Moore 

defaultSeed :: Int
defaultSeed = 42  

defaultLayout :: Layout
defaultLayout = []