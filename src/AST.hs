module AST where

data State        = Dead  | Alive
data Neighborhood = Moore | VonNeumann
data Neighbor     = | TopLeft     | Top    | TopRight  
                    | Left        | Self   | Right  
                    | BottomLeft  | Bottom | BottomRight

type Grid = [[State]]



data Exp a where
  
  -- Expresiones enteras
  Const       :: Int -> Exp Int
  Count       :: Exp State -> Exp Int
  UMinus      :: Exp Int -> Exp Int
  Plus        :: Exp Int -> Exp Int -> Exp Int
  Minus       :: Exp Int -> Exp Int -> Exp Int
  Times       :: Exp Int -> Exp Int -> Exp Int
  Div         :: Exp Int -> Exp Int -> Exp Int

  -- Expresiones booleanas
  BTrue       :: Exp Bool
  BFalse      :: Exp Bool
  IsNeighbor  :: Neighbor -> Exp State -> Exp Bool
  Lt          :: Exp Int -> Exp Int -> Exp Bool
  Gt          :: Exp Int -> Exp Int -> Exp Bool
  And         :: Exp Bool -> Exp Bool -> Exp Bool
  Or          :: Exp Bool -> Exp Bool -> Exp Bool
  Not         :: Exp Bool -> Exp Bool
  Eq          :: Exp Int -> Exp Int -> Exp Bool
  NEq         :: Exp Int -> Exp Int -> Exp Bool  

  -- Expresiones de estado
  SConst      :: State -> Exp State
  SIfThenElse :: Exp Bool -> Exp State -> Exp State -> Exp State
  SNot        :: Exp State -> Exp State
  WithProbability :: Double -> Exp State -> Exp State -> Exp State
  -- agregar probabilidades?

deriving instance Show (Exp a)
deriving instance Eq (Exp a)
