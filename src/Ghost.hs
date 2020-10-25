module Ghost where

import Model
import Graphics.Gloss

instance HasDirection Ghost where
  direction = ghostDirection
  updateMovementDirection direction ghost = ghost { ghostDirection = direction }

instance HasPosition Ghost where
  position = ghostPosition

-- Ghost renderable instance
instance Renderable Ghost where
  render gstate ghost = scaleAndTranslate gstate (currentGhostSprite ghost)
    where currentGhostSprite :: Ghost -> Picture
          currentGhostSprite ghost  | direction ghost == Model.None = head $ ghostSprites ghost
                                    | direction ghost == Model.Up = head $ ghostSprites ghost
                                    | direction ghost == Model.Down = ghostSprites ghost !! 1
                                    | direction ghost == Model.Left = ghostSprites ghost !! 2
                                    | direction ghost == Model.Right = ghostSprites ghost !! 3