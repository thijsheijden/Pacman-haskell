module Ghost where

import Model
import Graphics.Gloss
import Data.List
import Data.Ord
import System.Random
import Player

-- |One iteration update for ghost Blinky
updateBlinky :: GameState -> Ghost -> Ghost
updateBlinky gstate blinky@Ghost { ghostState = Trapped } = blinky
updateBlinky gstate blinky = blinky { ghostPosition = newPosition,
                                      targetPosition = newTargetPosition,
                                      ghostXSteps = newXSteps,
                                      ghostYSteps = newYSteps,
                                      ghostDirection = newMovementDirection }
  where
    newTargetPosition = (position . player) gstate
    possibleMovementDirections = possibleGhostDirections blinky (direction blinky) (position blinky) (board gstate) (round $ numberOfColumns gstate)
    newMovementDirection = fastestDirectionToTargetPosition gstate possibleMovementDirections (ghostState blinky) blinky newTargetPosition

    speed | ghostState blinky == Scared = 0.2
          | otherwise = 0.1

    newPositionAndSteps = updateGhostPosition newMovementDirection speed (position blinky) (stepsTaken blinky)

    newPosition = fst newPositionAndSteps
    newXSteps = (fst . snd) newPositionAndSteps
    newYSteps = (snd . snd) newPositionAndSteps

-- |One iteration update for ghost Inky
updateInky :: GameState -> Ghost -> Ghost
updateInky gstate inky = undefined

-- |One iteration update for ghost Clyde
updateClyde :: GameState -> Ghost -> Ghost
updateClyde gstate clyde = undefined

-- |One iteration update for ghost Pinky
updatePinky :: GameState -> Ghost -> Ghost
updatePinky gstate pinky = undefined

-- |Helper function that determines which direction the ghost can go into. Returns a list of possible directions and the point they lead to
possibleGhostDirections :: Ghost -> MovementDirection -> Point -> Board -> Int -> [(MovementDirection, Point)]
possibleGhostDirections ghost md position board numberOfColumns | md == Model.None  = onePointUp ++ onePointDown ++ onePointLeft ++ onePointRight
                                                                | md == Model.Up    = onePointLeft ++ onePointRight ++ onePointUp
                                                                | md == Model.Down  = onePointLeft ++ onePointRight ++ onePointDown
                                                                | md == Model.Left  = onePointUp ++ onePointDown ++ onePointLeft
                                                                | md == Model.Right = onePointUp ++ onePointDown ++ onePointRight
  where 
    onePointUp    | checkFieldAtPoint Model.Up && (allowedX . xSteps) ghost = [(Model.Up, (fst position, snd position + speed))]
                  | otherwise = []
    onePointDown  | checkFieldAtPoint Model.Down && (allowedX . xSteps) ghost = [(Model.Down, (fst position, snd position - speed))]
                  | otherwise = []
    onePointLeft  | checkFieldAtPoint Model.Left && (allowedY . ySteps) ghost = [(Model.Left, (fst position - speed, snd position))]
                  | otherwise = []
    onePointRight | checkFieldAtPoint Model.Right && (allowedY . ySteps) ghost = [(Model.Right, (fst position + speed, snd position))]
                  | otherwise = []

    checkFieldAtPoint dir = checkFieldInFuturePosition emptyOrPacdotHelper dir board numberOfColumns 0.05 position

    speed | ghostState ghost == Scared = 0.2
          | otherwise = 0.1

fastestDirectionToTargetPosition :: GameState -> [(MovementDirection, Point)] -> GhostState -> Ghost -> Point -> MovementDirection
fastestDirectionToTargetPosition _      [x] _      ghost targetPosition = fst x  -- If there is only one possible direction, to reduce unnecessary calculations
fastestDirectionToTargetPosition gstate xs  Scared ghost targetPosition = fst (xs !! fst (randomR (0, length xs - 1) (stdGen gstate)))  -- If the ghost is scared it will go in a random direction
fastestDirectionToTargetPosition gstate xs  state  ghost targetPosition = fst (snd (minimumBy (comparing fst) [(distanceBetweenTwoPoints targetPoint (snd x), x) | x <- xs]))
  where
    targetPoint | state == Dead = (10, 8)   -- TODO: Add a way to embed a ghost/pacman spawn location into the map
                | state == Scattering = home ghost
                | otherwise = targetPosition

-- |Update the position of a ghost. Returns a tuple containing (newPosition, (xSteps, ySteps))
updateGhostPosition :: MovementDirection -> Float -> Point -> (Int, Int) -> (Point, (Int, Int))
updateGhostPosition Model.None _  position stepsTaken = (position, stepsTaken)
updateGhostPosition Model.Up   speed (x, y) (xSteps, ySteps) = ((x, y + speed), (xSteps, ySteps + 1))
updateGhostPosition Model.Down   speed (x, y) (xSteps, ySteps) = ((x, y - speed), (xSteps, ySteps - 1))
updateGhostPosition Model.Left   speed (x, y) (xSteps, ySteps) = ((x - speed, y), (xSteps - 1, ySteps))
updateGhostPosition Model.Right   speed (x, y) (xSteps, ySteps) = ((x + speed, y), (xSteps + 1, ySteps))

-- Ghost HasDirection instance
instance HasDirection Ghost where
  direction = ghostDirection
  updateMovementDirection gstate direction ghost = undefined

-- Ghost HasPosition instance
instance HasPosition Ghost where
  position = ghostPosition
  stepsTaken ghost = (ghostXSteps ghost, ghostYSteps ghost)
  xSteps = ghostXSteps
  ySteps = ghostYSteps

-- Ghost renderable instance
instance Renderable Ghost where
  render gstate ghost = scaleAndTranslate gstate (currentGhostSprite ghost)
    where currentGhostSprite :: Ghost -> Picture
          currentGhostSprite ghost  | ghostState ghost  == Scared         = ghostSprites ghost !! 4
                                    | direction ghost   == Model.None     = head $ ghostSprites ghost
                                    | direction ghost   == Model.Up       = ghostSprites ghost !! 1
                                    | direction ghost   == Model.Down     = head $ ghostSprites ghost
                                    | direction ghost   == Model.Left     = ghostSprites ghost !! 2
                                    | direction ghost   == Model.Right    = ghostSprites ghost !! 3

-- Ghost Updateable instance
instance Updateable Ghost where
  update gstate blinky@Ghost { ghostName = Blinky }   = updateBlinky gstate blinky
  update gstate inky@Ghost { ghostName = Inky }       = updateInky gstate inky
  update gstate clyde@Ghost { ghostName = Clyde }     = updateClyde gstate clyde
  update gstate pinky@Ghost { ghostName = Pinky }     = updatePinky gstate pinky