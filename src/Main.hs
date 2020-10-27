module Main where

import Controller ( input, step )
import Model ( initialState )
import View ( view )
import System.Random ( newStdGen )

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), playIO )
import Graphics.Gloss ( loadBMP )

main :: IO ()
main = do

  -- Loading all the map bitmaps
  horizontal <- loadBMP "img/blocks/lines.bmp"
  vertical <- loadBMP "img/blocks/lines vertical.bmp"

  rightCorner <- loadBMP "img/blocks/left top corner.bmp"
  downCorner <- loadBMP "img/blocks/right top corner.bmp"
  leftCorner <- loadBMP "img/blocks/right bottom corner.bmp"
  upCorner <- loadBMP "img/blocks/left bottom corner.bmp"

  upConnector <- loadBMP "img/blocks/connector top.bmp"
  downConnector <- loadBMP "img/blocks/connector bottom.bmp"
  leftConnector <- loadBMP "img/blocks/connector left.bmp"
  rightConnector <- loadBMP "img/blocks/connector right.bmp"
  allConnector <- loadBMP "img/blocks/connector all.bmp"

  topRounded <- loadBMP "img/blocks/lines vertical top rounded.bmp"
  bottomRounded <- loadBMP "img/blocks/lines vertical bottom rounded.bmp"
  rightRounded <- loadBMP "img/blocks/lines horizontal right rounded.bmp"
  leftRounded <- loadBMP "img/blocks/lines horizontal left rounded.bmp"

  -- Loading the fruit bitmap
  fruit <- loadBMP "img/sprites/fruit.bmp"

  -- Loading all player bitmaps. There are two for every direction, one with a closed mouth and one with an open mouth
  playerUpClosed <- loadBMP "img/sprites/pacmanCU.bmp"
  playerDownClosed <- loadBMP "img/sprites/pacmanCD.bmp"
  playerLeftClosed <- loadBMP "img/sprites/pacmanCL.bmp"
  playerRightClosed <- loadBMP "img/sprites/pacmanCR.bmp"
  
  playerUpOpened <- loadBMP "img/sprites/pacmanOU.bmp"
  playerDownOpened <- loadBMP "img/sprites/pacmanOD.bmp"
  playerLeftOpened <- loadBMP "img/sprites/pacmanOL.bmp"
  playerRightOpened <- loadBMP "img/sprites/pacmanOR.bmp"

  -- Loading the ghost bitmaps. Each ghost has 4 bitmaps, one for each direction. And there is a single bitmap for the "scared" state and one for the "eaten" state
  blinkyUp <- loadBMP "img/sprites/blinkyU.bmp"
  blinkyDown <- loadBMP "img/sprites/blinkyD.bmp"
  blinkyLeft <- loadBMP "img/sprites/blinkyL.bmp"
  blinkyRight <- loadBMP "img/sprites/blinkyR.bmp"

  inkyUp <- loadBMP "img/sprites/inkyU.bmp"
  inkyDown <- loadBMP "img/sprites/inkyD.bmp"
  inkyLeft <- loadBMP "img/sprites/inkyL.bmp"
  inkyRight <- loadBMP "img/sprites/inkyR.bmp"

  clydeUp <- loadBMP "img/sprites/clydeU.bmp"
  clydeDown <- loadBMP "img/sprites/clydeD.bmp"
  clydeLeft <- loadBMP "img/sprites/clydeL.bmp"
  clydeRight <- loadBMP "img/sprites/clydeR.bmp"

  pinkyUp <- loadBMP "img/sprites/pinkyU.bmp"
  pinkyDown <- loadBMP "img/sprites/pinkyD.bmp"
  pinkyLeft <- loadBMP "img/sprites/pinkyL.bmp"
  pinkyRight <- loadBMP "img/sprites/pinkyR.bmp"

  scared <- loadBMP "img/sprites/scared.bmp"

  stdGen <- newStdGen
  map <- readFile "map.txt"
  
  playIO (InWindow "Pacman" (800, 720) (0, 0)) -- Or FullScreen
              black            -- Background color
              144               -- Frames per second
              (initialState stdGen map [horizontal, 
                                        vertical, 
                                        rightCorner, 
                                        downCorner, 
                                        leftCorner, 
                                        upCorner, 
                                        upConnector,
                                        downConnector,
                                        leftConnector,
                                        rightConnector,
                                        allConnector, 
                                        topRounded, 
                                        bottomRounded, 
                                        rightRounded, 
                                        leftRounded,
                                        fruit]
                                        [playerUpClosed,
                                        playerDownClosed,
                                        playerLeftClosed,
                                        playerRightClosed,
                                        playerUpOpened,
                                        playerDownOpened,
                                        playerLeftOpened,
                                        playerRightOpened]
                                        [[blinkyUp, blinkyDown, blinkyLeft, blinkyRight, scared],
                                        [inkyUp, inkyDown, inkyLeft, inkyRight, scared],
                                        [clydeUp, clydeDown, clydeLeft, clydeRight, scared],
                                        [pinkyUp, pinkyDown, pinkyLeft, pinkyRight, scared]])     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

