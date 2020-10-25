module Player where

import Model
import Graphics.Gloss
import Data.Fixed

instance HasDirection Player where
  direction = playerDirection
  updateMovementDirection gstate direction player   | canChangeDirectionNow gstate direction = player { playerDirection = direction }
                                                    | otherwise = player { playerFutureDirection = direction }
  -- updateMovementDirection gstate direction player = player { playerDirection = direction }

-- Checks if the movement direction can be changed right now or should be set as the future direction
-- Takes the current player position and checks if the numbers are essentially round
-- If this is the case it checks if the board has an Empty or Pacdot field in the new direction
-- If this is the case the function returns True, if not it returns False
canChangeDirectionNow :: GameState -> MovementDirection -> Bool
canChangeDirectionNow gstate md = canMovePosition md ((xSteps . player) gstate, (ySteps . player) gstate) && isFieldEmptyOrPacdot md (board gstate) ((round . numberOfColumns) gstate) 0.05 playerPosition
  where
    playerPosition = (position . player) gstate

    canMovePosition :: MovementDirection -> (Int, Int) -> Bool
    canMovePosition Model.None   _     = False      -- not sure if this should be True or False
    canMovePosition Model.Up    (x, _) = allowedX x
    canMovePosition Model.Down  (x, _) = allowedX x
    canMovePosition Model.Left  (_, y) = allowedY y
    canMovePosition Model.Right (_, y) = allowedY y

    allowedX :: Int -> Bool
    allowedX x = mod' x 10 == 0

    allowedY :: Int -> Bool
    allowedY y = mod' y 10 == 0

instance HasPosition Player where
  position          = playerPosition
  stepsTaken player = (xSteps player, ySteps player)

-- Pacman renderable instance
instance Renderable Player where
  render gstate player = scaleAndTranslate gstate (currentPlayerSprite player (animationState player))
    where currentPlayerSprite :: Player -> PlayerAnimationState -> Picture
          currentPlayerSprite player Open   | direction player == Model.None = playerSprites player !! 6
                                            | direction player == Model.Up = playerSprites player !! 4
                                            | direction player == Model.Down = playerSprites player !! 5
                                            | direction player == Model.Left = playerSprites player !! 6
                                            | direction player == Model.Right = playerSprites player !! 7
          currentPlayerSprite player Closed | direction player == Model.None = playerSprites player !! 6
                                            | direction player == Model.Up = head $ playerSprites player
                                            | direction player == Model.Down = playerSprites player !! 1
                                            | direction player == Model.Left = playerSprites player !! 2
                                            | direction player == Model.Right = playerSprites player !! 3