module Main where

import Controller
import Model
import View
import System.Random

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss

main :: IO ()
main = do

  -- Loading all images
  horizontal <- loadBMP "img/blocks/lines.bmp"
  vertical <- loadBMP "img/blocks/lines vertical.bmp"
  
  stdGen <- newStdGen
  map <- readFile "map.txt"
  
  playIO (InWindow "Pacman" (1280, 720) (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              (initialState stdGen map [horizontal, vertical])     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

