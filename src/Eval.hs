{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Eval
  (
    evalExp,
    runEval
  )
where

import AST
import qualified Data.Vector as V
import Control.Monad (liftM, ap)

import System.Random (StdGen, randomR)

-- Definimos la monada que utilizaremos para evaluar
-- Nos da acceso a los elementos necesarios para evaluar una expresion: la grilla, la posicion actual, el tipo de vecindad utilizada, y la semilla de aleatoriedad.
-- Utilizamos la mónada Maybe para permitir errores de división y además acarreamos la semilla.
newtype EvalM a = EvalM 
  { runEval :: Grid -> Position -> Neighborhood -> StdGen -> Maybe (a, StdGen) 
  }

instance Monad EvalM where
  return x = EvalM $ \_ _ _ gen -> Just (x, gen)
  m >>= f = EvalM $ \grid pos neigh gen ->
    case runEval m grid pos neigh gen of
      Just (x, gen') -> runEval (f x) grid pos neigh gen'
      Nothing -> Nothing

instance Applicative EvalM where
  pure x = EvalM $ \_ _ _ gen -> Just (x, gen)
  
  (<*>) f x = EvalM $ \grid pos neigh gen ->
    case runEval f grid pos neigh gen of
      Nothing -> Nothing
      Just (g, gen') ->
        case runEval x grid pos neigh gen' of
          Nothing -> Nothing
          Just (v, gen'') -> Just (g v, gen'')  

instance Functor EvalM where
  fmap = liftM


-- Y ahora funciones auxiliares
getGrid :: EvalM Grid
getGrid = EvalM $ \grid _ _ gen -> Just (grid, gen)

getPos :: EvalM Position
getPos = EvalM $ \_ pos _ gen -> Just (pos, gen)

getNeighborhood :: EvalM Neighborhood
getNeighborhood = EvalM $ \_ _ neigh gen -> Just (neigh, gen)

getCurrentState :: EvalM State
getCurrentState = do
  (y,x) <- getPos
  grid <- getGrid
  case (grid V.!? y) >>= (V.!? x) of
    Just state -> return state
    Nothing -> failEval

-- Devuelve un numero entre 0 y 1 para usar en withProbability, acarreando el nuevo StdGen
getRandom :: EvalM Double 
getRandom = EvalM $ \_ _ _ gen ->
  let (r, gen') = randomR (0, 1) gen
  in Just (r, gen')  

failEval :: EvalM a
failEval = EvalM $ \_ _ _ _ -> Nothing

evalProb :: Probability -> EvalM Bool
evalProb (Prob e) = do
    p <- evalExp e
    let boundedP = max 0 (min 1 p)  
    r <- getRandom
    return (r < boundedP)
evalProb Random = do
    r <- getRandom
    return (r < 0.5)

getNeighbors :: EvalM [State]
getNeighbors = do
  (y, x) <- getPos
  neighType <- getNeighborhood
  grid <- getGrid
  let rows = V.length grid
      cols = if rows > 0 then V.length (V.head grid) else 0
      offsets = case neighType of
                Moore -> [ (dy, dx) | dy <- [-1..1], dx <- [-1..1], (dy, dx) /= (0, 0) ]
                VonNeumann -> [ (-1, 0), (1, 0), (0, -1), (0, 1) ]
      
      -- | Aca hacemos que sea toroidal
      wrap coord maxCoord = (coord + maxCoord) `mod` maxCoord
      
      neighbors = 
        [ (wrap (y + dy) rows, wrap (x + dx) cols)
        | (dy, dx) <- offsets
        ]
  
  mapM getCell neighbors

-- Obtiene el valor en cierta celda de forma segura
getCell :: Position -> EvalM State
getCell (y, x) = do
  grid <- getGrid
  if y >= 0 && y < V.length grid &&
     x >= 0 && x < V.length (V.head grid)
  then return $ grid V.! y V.! x
  else failEval

evalExp :: Exp a -> EvalM a
evalExp (Const      n)  = return  n
evalExp (UMinus     n)  = evalUnary  negate n
evalExp (Plus   e0 e1)  = evalBin (+)   e0 e1
evalExp (Minus  e0 e1)  = evalBin (-)   e0 e1
evalExp (Times  e0 e1)  = evalBin (*)   e0 e1
evalExp (Div    e0 e1)  = do v0 <- evalExp e0
                             v1 <- evalExp e1
                             if v1 == 0 then failEval 
                                        else return (v0 / v1)

evalExp BTrue           = return True
evalExp BFalse          = return False 

evalExp (Not        e)  = evalUnary     not e
evalExp (Lt     e0 e1)  = evalBin (<)   e0 e1
evalExp (Gt     e0 e1)  = evalBin (>)   e0 e1
evalExp (And    e0 e1)  = evalBin (&&)  e0 e1
evalExp (Or     e0 e1)  = evalBin (||)  e0 e1
evalExp (Eq     e0 e1)  = evalBin (==)  e0 e1
evalExp (NEq    e0 e1)  = evalBin (/=)  e0 e1

evalExp EDead = return Dead
evalExp EAlive = return Alive
evalExp (IfThenElse e0 e1 e2) = do  cond <- evalExp e0
                                    v1   <- evalExp e1
                                    v2   <- evalExp e2
                                    return (if cond then v1 else v2)

evalExp (NotState e) = do state <- evalExp e
                          case state of
                            Alive -> return Dead
                            Dead  -> return Alive

evalExp (Count e) = do  neighbors <- getNeighbors 
                        state <- evalExp e
                        countMatchingNeighbors neighbors state

evalExp (WithProbability probExpr e1 e2) = do
  cond <- evalProb probExpr
  if cond then evalExp e1 
          else evalExp e2

evalExp (IsNeighbor dir stateExp) = do
  (y, x) <- getPos
  let (dy, dx) = case dir of
        TopLeft     -> (-1, -1)
        Top         -> (-1,  0)
        TopRight    -> (-1,  1)
        LeftNeigh   -> ( 0, -1)
        RightNeigh  -> ( 0,  1)
        BottomLeft  -> ( 1, -1)
        Bottom      -> ( 1,  0)
        BottomRight -> ( 1,  1)
        Self        -> ( 0,  0)
  neighborCell <- getCell (y + dy, x + dx)
  expectedState <- evalExp stateExp
  return (neighborCell == expectedState)

countMatchingNeighbors :: [State] -> State -> EvalM Double
countMatchingNeighbors [] _ = return 0
countMatchingNeighbors (cell:cells) state = do
  matchesRest <- countMatchingNeighbors cells state
  return $ if cell == state
           then matchesRest + 1 
           else matchesRest

evalBin :: (a -> a -> b) -> Exp a -> Exp a -> EvalM b
evalBin op e0 e1 = do v0 <- evalExp e0
                      v1 <- evalExp e1
                      return (op v0 v1)

evalUnary :: (a -> b) -> Exp a -> EvalM b
evalUnary op e = do v <- evalExp e
                    return (op v)