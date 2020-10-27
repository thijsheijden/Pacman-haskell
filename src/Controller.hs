-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Player
import Ghost
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Determine what kind of update we need and delegate it to the corresponding function
step :: Float -> GameState -> IO GameState
step secs gstate  | gameState gstate == Paused = return gstate                               -- If the game is paused return the previous gstate, no updates occur
                  | (lives . player) gstate == 0 || gameState gstate == GameOver = undefined -- Check if a highscore was achieved, if so ask the user to enter 3 characters and save their score to the highscore file
                  | (playerState . player) gstate == PlayerDead = undefined                  -- Respawn the player, reset the gamestate to the original gamestate except for the score, player lives etc
                  | otherwise = normalStep secs gstate                                       -- Continue the game with a normal update/step

-- |Perform one standard step in the game. The game is not paused, the player is not dead, the player has not won etc
normalStep :: Float -> GameState -> IO GameState
normalStep secs gstate = return $ gstate {  elapsedTime         = elapsedTime gstate + secs,
                                            elapsedBoardFrames  = elapsedBoardFrames gstate + 1,
                                            player              = newPlayer,
                                            blinky              = updateGhost gstate (blinky gstate),
                                            inky                = updateGhost gstate (inky gstate),
                                            clyde               = updateGhost gstate (clyde gstate),
                                            pinky               = updateGhost gstate (pinky gstate),
                                            score               = newScore,
                                            pacDotsOnBoard      = newPacdots,
                                            board               = newBoard,
                                            gameState           = newGameState }
                                              where
                                                newPlayer = update gstate (player gstate)
                                                newScorePacdotsAndBoard = updateScoreAndPacdots gstate (player gstate)

                                                newPacdots = (fst . fst) newScorePacdotsAndBoard
                                                newScore   = (snd . fst) newScorePacdotsAndBoard
                                                newBoard   = snd newScorePacdotsAndBoard

                                                newGameState  | newPacdots == 0 = GameOver
                                                              | otherwise = Playing

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
-- updateGhost :: GameState -> Ghost -> Ghost
-- updateGhost gstate ghost@(Ghost _ pos@(x,y) state _ spawn direc _ _ _ ) = ghost {ghostState = newstate, ghostPosition = newpos}
--   where
--     time    = elapsedBoardFrames gstate + 1
--     newstate| time `mod` 800 == 0   = Model.Scattering    -- not the way to do it, but changing state works
--             | time `mod` 400 == 0   = Model.Chasing
--             | otherwise             = state
--     step    | state == Scattering   = 0.2
--             | otherwise             = 0.1
--     newpos  | direc == Model.Up     = (x, y - step)
--             | direc == Model.Down   = (x, y + step)
--             | direc == Model.Left   = (x - step, y)
--             | direc == Model.Right  = (x + step, y)
--             | otherwise             = pos

-- |Update the game score and the number of pacdots left on the map. Check if the Player is now in a Pacdot field, and if this is the case update the board as well. Returns ((newPacdots, newScore), newBoard)
updateScoreAndPacdots :: GameState -> Player -> ((Int, Int), Board)
updateScoreAndPacdots gstate player | (isPacdot . fieldAtPosition (board gstate) (round $ numberOfColumns gstate) . position) player = ((pacDotsOnBoard gstate - 1, score gstate + 10), eatPacdot (board gstate) (position player))
                                    | (isPowerPacdot . fieldAtPosition (board gstate) (round $ numberOfColumns gstate) . position) player = ((pacDotsOnBoard gstate, score gstate), eatPacdot (board gstate) (position player))
                                    | otherwise = ((pacDotsOnBoard gstate, score gstate), board gstate)