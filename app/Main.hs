module Main where

import qualified Data.Vector as V
import System.Random (mkStdGen, StdGen)
import System.Console.ANSI (clearScreen)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Parser
import Eval
import AST

-- Para leer el archivo del directorio de tests
import System.FilePath (combine)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Console.ANSI (clearScreen, setCursorPosition, hideCursor, showCursor)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Uso: stack exec cellular-automaton-dsl-exe <test-file> [<generaciones>]"

        [testFile] -> do  
            let testPath = combine "test" testFile
            fileExists <- doesFileExist testPath
            if fileExists
                then do
                    contents <- readFile testPath
                    case parseFile contents of
                        Left err -> putStrLn $ "Error de parseo: " ++ show err
                        Right program -> withConsole $ runSimulation program 100
                else putStrLn $ "Archivo no encontrado: " ++ testPath

        (testFile:gensStr:_) -> do  
            let testPath = combine "test" testFile
            fileExists <- doesFileExist testPath
            case reads gensStr :: [(Int, String)] of
                [(gens, "")] -> 
                    if fileExists
                        then do
                            contents <- readFile testPath
                            case parseFile contents of
                                Left err -> putStrLn $ "Error de parseo: " ++ show err
                                Right program -> withConsole $ runSimulation program gens
                        else putStrLn $ "Archivo no encontrado: " ++ testPath
                _ -> putStrLn "Cantidad de generaciones inválida (debe ser un número entero)"

-- Funcion auxiliar para manejar la consola
withConsole :: IO a -> IO a
withConsole action = do
    hideCursor
    result <- action
    showCursor
    return result

runSimulation :: Program -> Int -> IO ()
runSimulation program@(Program size@(width, height) _ stepRule layout seed) steps = do
    let initialGrid = initializeGrid size layout
        gen = mkStdGen seed
    clearScreen
    loop program initialGrid gen steps 0
  where
    initializeGrid (w, h) aliveCells =
        V.generate h (\y -> 
            V.generate w (\x -> 
                if (x,y) `elem` aliveCells then Alive else Dead))

loop :: Program -> Grid -> StdGen -> Int -> Int -> IO ()
loop _ _ _ 0 _ = return ()
loop program grid gen remaining stepNum = do
    setCursorPosition 0 0
    putStr $ "Generación: " ++ show stepNum ++ "\n"
    displayGrid grid
    
    threadDelay 200000 
    
    let (newGrid, newGen) = stepGeneration program grid gen
    loop program newGrid newGen (remaining - 1) (stepNum + 1)

displayGrid :: Grid -> IO ()
displayGrid grid = do
    let border = "+" ++ replicate (V.length (V.head grid) * 2) '-' ++ "+"
    putStrLn border
    V.forM_ grid $ \row -> do
        putStr "|"
        V.forM_ row $ \cell -> 
            putStr $ case cell of
                Alive -> " ■"
                Dead  -> "  "
        putStrLn "|"
    putStr border 


stepGeneration :: Program -> Grid -> StdGen -> (Grid, StdGen)
stepGeneration (Program _ neighborhood stepRule _ _) grid gen =
    foldl' stepRow (V.empty, gen) [0..V.length grid - 1]
  where
    stepRow (accGrid, accGen) y =
        let (newRow, newGen) = foldl' (stepCell y) (V.empty, accGen) [0..V.length (V.head grid) - 1]
        in (accGrid `V.snoc` newRow, newGen)
    
    stepCell y (accRow, accGen) x =
        case runEval (evalExp stepRule) grid (y,x) neighborhood accGen of
            Just (newState, newGen) -> (accRow `V.snoc` newState, newGen)
            Nothing -> (accRow `V.snoc` Dead, accGen)