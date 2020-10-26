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
step secs gstate = return $ gstate {  elapsedTime   = elapsedTime gstate + secs,
                                      elapsedBoardFrames = elapsedBoardFrames gstate + 1,
                                      player  = updatePlayer gstate (player gstate),
                                      blinky  = updateGhost gstate (blinky gstate), 
                                      inky    = updateGhost gstate (inky gstate), 
                                      clyde   = updateGhost gstate (clyde gstate), 
                                      pinky   = updateGhost gstate (pinky gstate) }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate = gstate { player = newPlayer c (player gstate) }
                                                  where 
                                                    newPlayer :: Char -> Player -> Player
                                                    newPlayer 'w' player = updateMovementDirection gstate Model.Down player
                                                    newPlayer 's' player = updateMovementDirection gstate Model.Up player
                                                    newPlayer 'a' player = updateMovementDirection gstate Model.Left player
                                                    newPlayer 'd' player = updateMovementDirection gstate Model.Right player
                                                    newPlayer 'z' player = updateMovementDirection gstate Model.None player
                                                    newPlayer  _  player = updateMovementDirection gstate (direction player) player

-- other input
inputKey _ gstate = gstate

-- ghosts chase for 20 sec, scatter for 7
updateGhost :: GameState -> Ghost -> Ghost
updateGhost gstate ghost@(Ghost pos@(x,y) state _ direc _) = ghost {ghostState = newstate, ghostPosition = newpos}
  where
    time    = elapsedBoardFrames gstate + 1
    newstate| time `mod` 800 == 0   = Model.Scattering    -- not the way to do it, but changing state works
            | time `mod` 400 == 0   = Model.Chasing
            | otherwise             = state
    step    | state == Scattering   = 0.2
            | otherwise             = 0.1
    newpos  | direc == Model.Up     = (x, y - step)
            | direc == Model.Down   = (x, y + step)
            | direc == Model.Left   = (x - step, y)
            | direc == Model.Right  = (x + step, y)
            | otherwise             = pos
