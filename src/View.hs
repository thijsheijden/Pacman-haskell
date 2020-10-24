-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [renderBoard gstate, 
                            renderPlayer gstate, 
                            renderGhost (blinky gstate) gstate, 
                            renderGhost (inky gstate) gstate, 
                            renderGhost (clyde gstate) gstate, 
                            renderGhost (pinky gstate) gstate]

renderBoard :: GameState -> Picture
renderBoard gstate = pictures $ map (renderBoardRow gstate) (zip [0 ..] (board gstate))

renderBoardRow :: GameState -> (Float, Row) -> Picture
renderBoardRow gstate (yIndex, row) = pictures $ map (\(xIndex, fieldValue) -> render gstate fieldValue (xIndex, yIndex)) r
  where r = zip [0 ..] row

renderPlayer :: GameState -> Picture
renderPlayer gstate = render gstate (player gstate) (playerPosition $ player gstate)

renderGhost :: Ghost -> GameState -> Picture
renderGhost ghost gstate = render gstate ghost (ghostPosition ghost)