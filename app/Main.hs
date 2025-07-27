module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>))
import Parser (parseProgram)
import Control.Monad (forM_)

