module Player where

import Model
import Graphics.Gloss
import Data.Fixed

-- |Function which updates the player every game step
updatePlayer :: GameState -> Player -> Player
updatePlayer gstate player = player { playerPosition = newPosition,
                                      playerDirection = newMovementDirection,
                                      playerFutureDirection = newFutureMovementDirection,
                                      xSteps = newXSteps,
                                      ySteps = newYSteps,
                                      playerAnimationState = newAnimationState,
                                      elapsedPlayerFrames = elapsedPlayerFrames player + 1,
                                      playerState = newState,
                                      playerStateTimer = newStateTimer }
  where
    playerMovementObject = updatePlayerPosition (position player) (stepsTaken player) (round $ numberOfColumns gstate) (board gstate) (direction player) (playerFutureDirection player)
    newPosition = (fst . fst) playerMovementObject
    
    newXSteps = (fst . snd) playerMovementObject
    newYSteps = (snd . snd) playerMovementObject

    -- If the future movement direction was possible, replace the old movement direction with the future movement direction
    newMovementDirection  | (snd . fst) playerMovementObject = playerFutureDirection player
                          | otherwise = direction player
    
    -- If the player is now moving in the future direction, set the future direction to None. If not do nothing.
    newFutureMovementDirection  | (snd . fst) playerMovementObject = Model.None
                                | otherwise = playerFutureDirection player

    -- The new player animation state, used for animating
    newAnimationState = updatePlayerAnimationState (elapsedFrames player) (playerAnimationState player)

    -- The new state of the player
    newPlayerStateAndTimer = updatePlayerState gstate (playerState player) (playerStateTimer player) (position player)
    newState = snd newPlayerStateAndTimer
    newStateTimer = fst newPlayerStateAndTimer

{-|
  Update the position of the player
  Returns the new position of the player and a boolean which denotes whether to remove the future direction (because the player went in that direction)
  Returns the xSteps and ySteps after the move as second pair of values
-}
updatePlayerPosition :: Point -> (Int, Int) -> Int -> Board -> MovementDirection -> MovementDirection -> ((Point, Bool), (Int, Int))
updatePlayerPosition (x, y) (xSteps, ySteps) _               _     Model.None _   = (((x, y), False), (xSteps, ySteps))
updatePlayerPosition (x, y) (xSteps, ySteps) numberOfColumns board md         fmd | fmd == Model.Up && allowedX xSteps && canMoveInDirection fmd = (((x, y + 0.1), True), (xSteps, ySteps + 1))
                                                                                  | fmd == Model.Down && allowedX xSteps && canMoveInDirection fmd = (((x, y - 0.1), True), (xSteps, ySteps - 1))
                                                                                  | fmd == Model.Left && allowedY ySteps && canMoveInDirection fmd = (((x - 0.1, y), True), (xSteps - 1, ySteps))
                                                                                  | fmd == Model.Right && allowedY ySteps && canMoveInDirection fmd = (((x + 0.1, y), True), (xSteps + 1, ySteps))

                                                                                  -- If we can't go into the future movement direction continue in the normal movement direction, if possible
                                                                                  | md == Model.Up && allowedX xSteps && canMoveInDirection md = (((x, y + 0.1), False), (xSteps, ySteps + 1))
                                                                                  | md == Model.Down && allowedX xSteps && canMoveInDirection md = (((x, y - 0.1), False), (xSteps, ySteps - 1))
                                                                                  | md == Model.Left && allowedY ySteps && canMoveInDirection md = (((x - 0.1, y), False), (xSteps - 1, ySteps))
                                                                                  | md == Model.Right && allowedY ySteps && canMoveInDirection md = (((x + 0.1, y), False), (xSteps + 1, ySteps))

                                                                                  | otherwise = (((x, y), False), (xSteps, ySteps))

                                                                                    where
                                                                                      canMoveInDirection direction = isFieldEmptyOrPacdot direction board numberOfColumns 0.05 (x, y)

-- |Determine if pacman is powered and what his updated powered timer is. If he is in the field of a power pacdot he will become powered for 20 seconds. Returns (poweredTimer, powered)
-- TODO: Add a collision detection with the ghosts to be able to change the player state to dead
updatePlayerState :: GameState -> PlayerState -> Float -> Point -> (Float, PlayerState)
updatePlayerState gstate PlayerBoosted poweredTimer position  | poweredTimer < 0 = (0, PlayerAlive) -- If the state timer runs out, go back to the alive state
                                                              | (isPowerPacdot . fieldAtPosition (board gstate) (round $ numberOfColumns gstate)) position = (20, PlayerBoosted) -- If the player runs over another powered pacdot, reset the timer
                                                              | otherwise = (poweredTimer - (1/40), PlayerBoosted)  -- Otherwise remove 1 frame worth of time from the timer
updatePlayerState gstate PlayerAlive   _            position  | (isPowerPacdot . fieldAtPosition (board gstate) (round $ numberOfColumns gstate)) position = (20, PlayerBoosted)  -- If the player runs over a powered pacdot change the state to powered
                                                              | otherwise = (0, PlayerAlive)  -- If no powered pacdot is ran over keep the same state

-- |Update the player animation state. Takes the elapsedFrames and current animation state and returns the new animation state
updatePlayerAnimationState :: Int -> PlayerAnimationState -> PlayerAnimationState
updatePlayerAnimationState elapsedFrames animationState | elapsedFrames `mod` 10 == 0 = nextState animationState
                                                        | otherwise = animationState

instance HasDirection Player where
  direction = playerDirection
  updateMovementDirection gstate direction player   | canChangeDirectionNow gstate direction = player { playerDirection = direction }
                                                    | otherwise = player { playerFutureDirection = direction }
  -- updateMovementDirection gstate direction player = player { playerDirection = direction }

{-|
  Checks if the movement direction can be changed right now or should be set as the future direction
  Takes the current player position and checks if the numbers are essentially round
  If this is the case it checks if the board has an Empty or Pacdot field in the new direction
  If this is the case the function returns True, if not it returns False
-}
canChangeDirectionNow :: GameState -> MovementDirection -> Bool
canChangeDirectionNow gstate md = canMovePosition md ((stepsTaken . player) gstate) && isFieldEmptyOrPacdot md (board gstate) ((round . numberOfColumns) gstate) 1 playerPosition
  where
    playerPosition = (position . player) gstate

    canMovePosition :: MovementDirection -> (Int, Int) -> Bool
    canMovePosition Model.None  (_     , _) = True
    canMovePosition Model.Up    (xSteps, _) = allowedX xSteps
    canMovePosition Model.Down  (xSteps, _) = allowedX xSteps
    canMovePosition Model.Left  (_, ySteps) = allowedY ySteps
    canMovePosition Model.Right (_, ySteps) = allowedY ySteps

{-|
  Helper method which tell whether the player is in the center of a square
  The player moves 0.1 per iteration and a single block is size 1
  Thus when the user has made 10 steps he is in the center of the next block
  Mod' used due to the possibility of negative numbers
-}
allowedX :: Int -> Bool
allowedX xSteps = mod' xSteps 10 == 0

{-|
  Helper method which tell whether the player is in the center of a square
  The player moves 0.1 per iteration and a single block is size 1
  Thus when the user has made 10 steps he is in the center of the next block
  Mod' used due to the possibility of negative numbers
-}
allowedY :: Int -> Bool
allowedY ySteps = mod' ySteps 10 == 0

-- Pacman HasPosition instance
instance HasPosition Player where
  position          = playerPosition
  stepsTaken player = (xSteps player, ySteps player)

-- Pacman Renderable instance
instance Renderable Player where
  render gstate player = scaleAndTranslate gstate (currentPlayerSprite player (playerAnimationState player))
    where currentPlayerSprite :: Player -> PlayerAnimationState -> Picture
          currentPlayerSprite player Open   | direction player == Model.None  = playerSprites player !! 6
                                            | direction player == Model.Up    = playerSprites player !! 5
                                            | direction player == Model.Down  = playerSprites player !! 4
                                            | direction player == Model.Left  = playerSprites player !! 6
                                            | direction player == Model.Right = playerSprites player !! 7
          currentPlayerSprite player Closed | direction player == Model.None  = playerSprites player !! 6
                                            | direction player == Model.Up    = playerSprites player !! 1
                                            | direction player == Model.Down  = head $ playerSprites player
                                            | direction player == Model.Left  = playerSprites player !! 2
                                            | direction player == Model.Right = playerSprites player !! 3

-- PlayerAnimationState AnimationState instance
instance AnimationState PlayerAnimationState where
  nextState Open = Closed
  nextState Closed = Open

-- Pacman Animatable instance
instance Animatable Player where
  elapsedFrames = elapsedPlayerFrames
