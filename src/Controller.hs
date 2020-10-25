-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Player
import Ghost
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate {  elapsedTime = elapsedTime gstate + secs,
                                      elapsedFrames = elapsedFrames gstate + 1,
                                      player = updatePlayer (player gstate),
                                      blinky = updateGhost (blinky gstate), 
                                      inky = updateGhost (inky gstate), 
                                      clyde = updateGhost (clyde gstate), 
                                      pinky = updateGhost (pinky gstate) }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate = gstate { player = newPlayer c (player gstate) }
                                                    where 
                                                      newPlayer :: Char -> Player -> Player
                                                      newPlayer 'w' player = updateMovementDirection Model.Up player
                                                      newPlayer 's' player = updateMovementDirection Model.Down player
                                                      newPlayer 'a' player = updateMovementDirection Model.Left player
                                                      newPlayer 'd' player = updateMovementDirection Model.Right player
                                                      newPlayer _ player = updateMovementDirection (direction player) player

-- other input
inputKey _ gstate = gstate

updateGhost :: Ghost -> Ghost
updateGhost ghost = ghost

updatePlayer :: Player -> Player
updatePlayer player = player