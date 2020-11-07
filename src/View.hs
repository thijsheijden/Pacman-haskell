-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player
import Ghost

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate@GameState { gameState = Restarting } = renderHighscoreView gstate
viewPure gstate@GameState { gameState = Paused } = renderPauseScreen gstate
viewPure gstate = pictures [renderBoard gstate, 
                            renderPlayer (player gstate) gstate, 
                            renderGhost (blinky gstate) gstate, 
                            renderGhost (inky gstate) gstate, 
                            renderGhost (clyde gstate) gstate, 
                            renderGhost (pinky gstate) gstate,
                            renderElapsedTime gstate (elapsedTime gstate),
                            renderCurrentScore gstate,
                            renderLives gstate]

-- |Render the board
renderBoard :: GameState -> Picture
renderBoard gstate = pictures $ zipWith (curry (renderBoardRow gstate)) [0 .. ] (board gstate)

-- |Render a single row of the board
renderBoardRow :: GameState -> (Float, Row) -> Picture
renderBoardRow gstate (yIndex, row) = pictures $ map (\(xIndex, fieldValue) -> render gstate fieldValue (xIndex, yIndex)) r
  where r = zip [0 ..] row

-- |Render the player (Pac-Man)
renderPlayer :: Player -> GameState -> Picture
renderPlayer player gstate = render gstate player (position player)

-- |Render a single ghost
renderGhost :: Ghost -> GameState -> Picture
renderGhost ghost gstate = render gstate ghost (position ghost)

-- |Render the elapsed time on the screen
renderElapsedTime :: GameState -> Float -> Picture
renderElapsedTime gstate et = scaleAndTranslate gstate ((color white . text) txt) (21, 1)
  where
    txt = show (round et) ++ "s"

-- |Render current score
renderCurrentScore :: GameState -> Picture
renderCurrentScore gstate = scaleAndTranslate gstate ((color white . text) txt) (21, 2.5)
  where 
    txt = "Score: " ++ show (score gstate)

-- |Render Pac-Mans remaining lives
renderLives :: GameState -> Picture
renderLives gstate = scaleAndTranslate gstate ((color white . text) txt) (-6, 1)
  where 
    txt = "Lives: " ++ show (lives (player gstate))

-- |Render the highscores on the game over screen from the txt file
renderHighscoreView :: GameState -> Picture
renderHighscoreView gstate = pictures $ [scaleAndTranslate gstate ((color magenta . text) "Highscores - any key to restart") (numberOfColumns gstate / 2 - 11, 2)] ++ renderScore (highScores gstate) 4 gstate

renderScore :: [String] -> Float -> GameState -> [Picture]
renderScore [] n gstate = [blank]
renderScore (x:xs) n gstate = scaleAndTranslate gstate ((color magenta . text) x) (numberOfColumns gstate / 2 - (0.20 * fromIntegral (length x)), n) : (renderScore xs (n + 3) gstate)

-- |Render the pause screen
renderPauseScreen :: GameState -> Picture
renderPauseScreen gstate = scaleAndTranslate gstate ((color magenta . text) "Paused") (numberOfColumns gstate / 2 - 2.5, 5)