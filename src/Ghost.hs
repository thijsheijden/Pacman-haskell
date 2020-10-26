module Ghost where

import Model
import Graphics.Gloss

-- Ghost HasDirection instance
instance HasDirection Ghost where
  direction = ghostDirection
  updateMovementDirection gstate direction ghost = ghost { ghostDirection = direction }

-- Ghost HasPosition instance
instance HasPosition Ghost where
  position = ghostPosition
  stepsTaken ghost = (1, 1)

-- Ghost renderable instance
instance Renderable Ghost where
  render gstate ghost = scaleAndTranslate gstate (currentGhostSprite ghost)
    where currentGhostSprite :: Ghost -> Picture
          currentGhostSprite ghost  | ghostState ghost  == Scared         = ghostSprites ghost !! 4
                                    | direction ghost   == Model.None     = head $ ghostSprites ghost
                                    | direction ghost   == Model.Up       = head $ ghostSprites ghost
                                    | direction ghost   == Model.Down     = ghostSprites ghost !! 1
                                    | direction ghost   == Model.Left     = ghostSprites ghost !! 2
                                    | direction ghost   == Model.Right    = ghostSprites ghost !! 3