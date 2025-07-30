{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST
  ( -- * Tipos principales
    Program(..)
  , State(..)
  , Neighborhood(..)
  , Probability(..)
  , Neighbor(..)
  , Exp(..)
    
    -- * Alias de tipos
  , Position
  , Grid
  , GridSize
  , Layout
  , Step
  , Seed
  ) where

import qualified Data.Vector as V

-------------------------------------------------------------------------------
-- * Tipos basicos del autómata celular
-------------------------------------------------------------------------------

-- | Estado posible de una célula
data State = Dead   -- ^ Célula muerta
           | Alive  -- ^ Célula viva
           deriving (Show, Eq)

-- | Posición en la grilla (fila, columna)
type Position = (Int, Int)

-- | Tamaño de la grilla (ancho, alto)
type GridSize = (Int, Int)

-- | Grilla representada como vector de vectores
type Grid = V.Vector (V.Vector State)

-- | Configuración inicial de células vivas
type Layout = [Position]

-- | Semilla para generación de números aleatorios en caso de utilizarlos
type Seed = Int

-- | Tipo de vecindad para las reglas de evolucion
data Neighborhood = Moore      -- ^ Vecindad de Moore (8 vecinos)
                  | VonNeumann -- ^ Vecindad de Von Neumann (4 vecinos)
                  deriving (Show)

-- | Posiciones relativas de los vecinos
data Neighbor     =   TopLeft     | Top    | TopRight  
                    | LeftNeigh   | Self   | RightNeigh  
                    | BottomLeft  | Bottom | BottomRight 
                    deriving (Show, Eq)

-- | Representación de probabilidades para reglas estocásticas
data Probability = Prob (Exp Double) -- ^ Probabilidad explícita (0.0 a 1.0)
                 | Random            -- ^ Probabilidad aleatoria (50%)
                 deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Expresiones del lenguaje (GADT)
-------------------------------------------------------------------------------

data Exp a where
  
  -- Expresiones enteras
  Const           :: Double -> Exp Double
  Count           :: Exp State -> Exp Double
  UMinus          :: Exp Double -> Exp Double
  Plus            :: Exp Double -> Exp Double -> Exp Double
  Minus           :: Exp Double -> Exp Double -> Exp Double
  Times           :: Exp Double -> Exp Double -> Exp Double
  Div             :: Exp Double -> Exp Double -> Exp Double

  -- Expresiones booleanas
  BTrue           :: Exp Bool
  BFalse          :: Exp Bool
  IsNeighbor      :: Neighbor -> Exp State -> Exp Bool
  Lt              :: Exp Double -> Exp Double -> Exp Bool
  Gt              :: Exp Double -> Exp Double -> Exp Bool
  And             :: Exp Bool -> Exp Bool -> Exp Bool
  Or              :: Exp Bool -> Exp Bool -> Exp Bool
  Not             :: Exp Bool -> Exp Bool
  Eq              :: Exp Double -> Exp Double -> Exp Bool
  NEq             :: Exp Double -> Exp Double -> Exp Bool  

  -- Expresiones de estado
  EDead           :: Exp State
  EAlive          :: Exp State
  IfThenElse      :: Exp Bool -> Exp State -> Exp State -> Exp State
  NotState        :: Exp State -> Exp State
  WithProbability :: Probability -> Exp State -> Exp State -> Exp State

deriving instance Show (Exp a)
deriving instance Eq (Exp a)

-- | Regla de evolución (expresión que devuelve State)
type Step = Exp State

-- | Programa completo del autómata celular
data Program = Program
  { gridSize      :: GridSize      -- ^ Tamaño de la grilla
  , neighborhood  :: Neighborhood  -- ^ Tipo de vecindad
  , stepRule      :: Step          -- ^ Regla de evolución
  , initialLayout :: Layout        -- ^ Posiciones iniciales vivas
  , seed          :: Seed          -- ^ Semilla aleatoria
  }
  | InvalidProgram  -- ^ Programa inválido 
  deriving (Show)

