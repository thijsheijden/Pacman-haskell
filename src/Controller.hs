-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Player
import Ghost
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Directory
import System.Time.Extra
import Data.List
import Debug.Trace
import Control.Monad

-- |Determine what kind of update we need and delegate it to the corresponding function
step :: Float -> GameState -> IO GameState
step secs gstate  | gameState gstate == Paused || gameState gstate == Restarting = return gstate                                            -- If the game is paused return the previous gstate, no updates occur
                  | (lives . player) gstate <= 0 = writeHighScore gstate  -- Check if a highscore was achieved, if so ask the user to enter 3 characters and save their score to the highscore file
                  | (playerState . player) gstate == PlayerDead = resetStep gstate                   -- Respawn the player, reset the gamestate to the original gamestate except for the score, player lives etc
                  | otherwise = normalStep secs gstate                                                    -- Continue the game with a normal update/step

-- |Perform one standard step in the game. The game is not paused, the player is not dead, the player has not won etc
normalStep :: Float -> GameState -> IO GameState
normalStep secs gstate = return $ gstate {  elapsedTime         = elapsedTime gstate + secs,
                                            elapsedBoardFrames  = elapsedBoardFrames gstate + 1,
                                            player              = update gstate (player gstate),
                                            blinky              = update gstate (blinky gstate),
                                            inky                = update gstate (inky gstate),
                                            clyde               = update gstate (clyde gstate),
                                            pinky               = update gstate (pinky gstate),
                                            score               = newScore,
                                            pacDotsOnBoard      = newPacdots,
                                            board               = newBoard,
                                            gameState           = newGameState,
                                            scatterTimer        = newScatterTimer,
                                            ghostStates         = newGhostStates,
                                            stdGen              = newGen,
                                            multiplier          = newMultiplier }
                                              where
                                                ghosts = [blinky gstate, inky gstate, clyde gstate, pinky gstate]

                                                newScorePacdotsAndBoard = updateScoreAndPacdots gstate (player gstate)

                                                newPacdots = (fst . fst) newScorePacdotsAndBoard - fruitPacdotAdjustment
                                                newScore   = (snd . fst) newScorePacdotsAndBoard + playerEatGhost (player gstate) ghosts newMultiplier
                                                (fruitPacdotAdjustment, newBoard, newGen) = (newFruit . snd) newScorePacdotsAndBoard

                                                newGameState  | newPacdots <= 0 = GameOver
                                                              | lives (player gstate) <= 0 = GameOver
                                                              | otherwise = Playing
                                                
                                                newScatterTimerAndGhostStates | ghostStates gstate == Chasing && scatterTimer gstate > 20 = (0, Scattering)
                                                                              | ghostStates gstate == Scattering && scatterTimer gstate > 7 = (0, Chasing)
                                                                              | otherwise = (scatterTimer gstate + secs, ghostStates gstate)

                                                newScatterTimer = fst newScatterTimerAndGhostStates
                                                newGhostStates = snd newScatterTimerAndGhostStates

                                                newFruit x  | scatterTimer gstate > 20 = addFruit gstate x
                                                            | otherwise = (0, x, stdGen gstate)

                                                newMultiplier = 1 + calculateMultiplier ghosts

resetStep :: GameState -> IO GameState
resetStep gstate = return gstate {
                                  player  = resetPlayer (player gstate),
                                  blinky  = resetGhost (blinky gstate),
                                  inky    = resetGhost (inky gstate),
                                  clyde   = resetGhost (clyde gstate),
                                  pinky   = resetGhost (pinky gstate),
                                  elapsedTime = 0,
                                  elapsedBoardFrames = 0,
                                  multiplier = 1,
                                  scatterTimer = 0
                                  }
  where
    resetPlayer player = player { lives = lives player - 1,
                                  playerPosition = spawnLocation gstate,
                                  playerDirection = Model.None,
                                  playerFutureDirection = Model.None,
                                  playerState = PlayerAlive,
                                  playerXSteps = 0,
                                  playerYSteps = 0 }
    resetGhost ghost = ghost  { ghostPosition = spawn ghost,
                                ghostState = Trapped,
                                ghostXSteps = 0,
                                ghostYSteps = 0 }

-- |Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- The p key is used to toggle Pause
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') _ _ _) gstate | gameState gstate == Playing = gstate { gameState = Paused}
                                            | otherwise = gstate { gameState = Playing }
inputKey (EventKey (Char _) _ _ _) gstate@GameState { gameState = Restarting } = gstate { gameState = Playing }
inputKey (EventKey (Char c) _ _ _) gstate = gstate { player = newPlayer c (player gstate) }
                                                  where 
                                                    newPlayer :: Char -> Player -> Player
                                                    newPlayer 'w' player = updateMovementDirection gstate Model.Down player
                                                    newPlayer 's' player = updateMovementDirection gstate Model.Up player
                                                    newPlayer 'a' player = updateMovementDirection gstate Model.Left player
                                                    newPlayer 'd' player = updateMovementDirection gstate Model.Right player
                                                    -- newPlayer 'p' player = updateMovementDirection gstate Model.None player
                                                    newPlayer  _  player = updateMovementDirection gstate (direction player) player

-- other input
inputKey _ gstate = gstate

-- |Update the game score and the number of pacdots left on the map. Check if the Player is now in a Pacdot field, and if this is the case update the board as well. Returns ((newPacdots, newScore), newBoard)
updateScoreAndPacdots :: GameState -> Player -> ((Int, Int), Board)
updateScoreAndPacdots gstate player | (isPacdot . fieldAtPosition (board gstate) (round $ numberOfColumns gstate) . position) player = ((pacDotsOnBoard gstate - 1, score gstate + 10), eatPacdot (board gstate) (position player))
                                    | (isPowerPacdot . fieldAtPosition (board gstate) (round $ numberOfColumns gstate) . position) player = ((pacDotsOnBoard gstate, score gstate), eatPacdot (board gstate) (position player))
                                    | (isFruit . fieldAtPosition (board gstate) (round $ numberOfColumns gstate) . position) player = ((pacDotsOnBoard gstate, score gstate + 100), eatPacdot (board gstate) (position player))
                                    | otherwise = ((pacDotsOnBoard gstate, score gstate), board gstate)

-- |Create a new piece of fruit and place it somewhere on the map
addFruit :: GameState -> Board -> (Int, Board, StdGen)
addFruit gstate board = (scoreAdjustment, changeFieldAtPosition board Fruit position, newGen)
  where
    positionAndStdGen = generateFruitPosition (numberOfColumns gstate) (numberOfRows gstate) board (stdGen gstate)
    position = fst positionAndStdGen
    newGen = snd positionAndStdGen
    scoreAdjustment = if isPacdot $ fieldAtPosition board (round $ numberOfColumns gstate) position then 1 else 0

generateFruitPosition :: Float -> Float -> Board -> StdGen -> (Point, StdGen)
generateFruitPosition nColumns nRows board stdGen   | (emptyOrPacdotHelperFruit . fieldAtPosition board (round nColumns)) (fst randomX, fst randomY) = ((fst randomX, fst randomY), snd randomX)
                                                    | otherwise = trace "fruit" $ generateFruitPosition nColumns nRows board (snd randomY)
                                                      where
                                                        randomX = randomR (1, nColumns - 2) stdGen
                                                        randomY = randomR (1, nRows - 2) (snd randomX)

-- |Write a highscore to the txt file, only show the top 5
writeHighScore :: GameState -> IO GameState
writeHighScore gstate = do

  let newScores = unlines $ take 5 $ insertAndSaveHighscore (score gstate) (highScores gstate)
  writeFile "highscores2.txt" newScores
  renameFile "highscores2.txt" "highscores.txt"

  map <- readFile "map.txt"

  return (initialState (stdGen gstate) map (pics gstate) (playerSprites $ player gstate) (ghostSpritesS gstate) (lines newScores) 0 Restarting)

-- non exhaustive error vvv
insertAndSaveHighscore :: Int -> [String] -> [String]
insertAndSaveHighscore score []       = [show score]
insertAndSaveHighscore score [s]      | score > (read s :: Int) = show score : [s]
                                      | otherwise               = [s] ++ [show score]
insertAndSaveHighscore score l@(s:ss) | score > (read s :: Int) = [show score] ++ l
                                      | otherwise               = s : insertAndSaveHighscore score ss

calculateMultiplier :: [Ghost] -> Int
calculateMultiplier [] = 0
calculateMultiplier (ghost:ghosts)  | ghostState ghost == Dead = 1 + calculateMultiplier ghosts
                                    | otherwise = 0 + calculateMultiplier ghosts

playerEatGhost :: Player -> [Ghost] -> Int -> Int
playerEatGhost player [] multiplier = 0
playerEatGhost player (ghost:ghosts) multiplier | canEatGhost = multiplier * 200 + playerEatGhost player ghosts multiplier
                                                | otherwise = playerEatGhost player ghosts multiplier
  where
    canEatGhost = collision player ghost && playerState player == PlayerBoosted && ghostState ghost == Scared