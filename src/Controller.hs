-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate {  elapsedTime = elapsedTime gstate + secs, 
                                      player = updatePlayer (player gstate),
                                      blinky = updateGhost (blinky gstate), 
                                      inky = updateGhost (inky gstate), 
                                      clyde = updateGhost (clyde gstate), 
                                      pinky = updateGhost (pinky gstate) }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate
inputKey _ gstate = gstate -- Otherwise keep the same


updateGhost :: Ghost -> Ghost
updateGhost ghost = ghost { ghostDirection = nextDir (ghostDirection ghost) }

updatePlayer :: Player -> Player
updatePlayer player = player { playerDirection = nextDir (playerDirection player) }

nextDir :: MovementDirection -> MovementDirection
nextDir None = Model.Up
nextDir Model.Up = Model.Right
nextDir Model.Right = Model.Down
nextDir Model.Down = Model.Left
nextDir Model.Left = Model.Up