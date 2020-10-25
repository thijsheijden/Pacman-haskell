module Player where

import Model
import Graphics.Gloss

instance HasDirection Player where
  direction = playerDirection
  updateMovementDirection direction player = player { playerDirection = direction}

instance HasPosition Player where
  position = playerPosition

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