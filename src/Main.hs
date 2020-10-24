module Main where

import Controller ( input, step )
import Model ( initialState )
import View ( view )
import System.Random ( newStdGen )

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), playIO )
import Graphics.Gloss ( black, loadBMP, Display(InWindow) )

main :: IO ()
main = do

  -- Loading all images
  horizontal <- loadBMP "img/blocks/lines.bmp"
  vertical <- loadBMP "img/blocks/lines vertical.bmp"
  rightCorner <- loadBMP "img/blocks/left top corner.bmp"
  downCorner <- loadBMP "img/blocks/right top corner.bmp"
  leftCorner <- loadBMP "img/blocks/right bottom corner.bmp"
  upCorner <- loadBMP "img/blocks/left bottom corner.bmp"
  upConnector <- loadBMP "img/blocks/connector top.bmp"
  topRounded <- loadBMP "img/blocks/lines vertical top rounded.bmp"
  bottomRounded <- loadBMP "img/blocks/lines vertical bottom rounded.bmp"
  rightRounded <- loadBMP "img/blocks/lines horizontal right rounded.bmp"
  leftRounded <- loadBMP "img/blocks/lines horizontal left rounded.bmp"
  
  stdGen <- newStdGen
  map <- readFile "map.txt"
  
  playIO (InWindow "Pacman" (1280, 720) (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              (initialState stdGen map [horizontal, vertical, rightCorner, downCorner, leftCorner, upCorner, upConnector, topRounded, bottomRounded, rightRounded, leftRounded])     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

