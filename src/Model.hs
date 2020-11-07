-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random
import Graphics.Gloss
import Data.Maybe
import Data.List
import Control.Lens hiding (Empty)
import Data.Fixed

secsBetweenCycles :: Float
secsBetweenCycles = 4

--------------------------------------------------
---------------INITIAL STATES---------------------
--------------------------------------------------

-- |Creating the initial gamestate record object
initialState :: StdGen -> String -> [Picture] -> [Picture] -> [[Picture]] -> [String] -> Float -> State-> GameState
initialState stdGen map pics playerSprites ghostSprites highScores elapsedSeconds s = GameState { player = initialPlayer playerSprites spawnPosition,
                                                                                                  stdGen = stdGen,
                                                                                                  score = 0,
                                                                                                  gameState = s,
                                                                                                  board = board, 
                                                                                                  pics = pics, 
                                                                                                  gridSize = gridSize, 
                                                                                                  numberOfColumns = numberOfColumns, 
                                                                                                  numberOfRows = numberOfRows,
                                                                                                  multiplier = 1,
                                                                                                  elapsedTime = elapsedSeconds,
                                                                                                  elapsedBoardFrames = 0,
                                                                                                  blinky = initialGhost (head ghostSprites) blinkyPosition blinkyHome Blinky 5,
                                                                                                  inky = initialGhost (ghostSprites !! 1) inkyPosition inkyHome Inky 7,
                                                                                                  clyde = initialGhost (ghostSprites !! 2) clydePosition clydeHome Clyde 9,
                                                                                                  pinky = initialGhost (ghostSprites !! 3) pinkyPosition pinkyHome Pinky 11,
                                                                                                  pacDotsOnBoard = numberOfPacdotsOnTheBoard board,
                                                                                                  spawnLocation = spawnPosition,
                                                                                                  scatterTimer = 0,       -- Must be changed to be blinky release + 20 seconds
                                                                                                  ghostStates = Chasing,
                                                                                                  ghostSpritesS = ghostSprites,
                                                                                                  highScores = highScores
                                                                                                }
  where
    board = createBoard map
    numberOfColumns = fromIntegral $ length $ head board
    numberOfRows = fromIntegral $ length board
    gridSize = calculateGridSize numberOfColumns numberOfRows
    blinkyPosition = findFieldPositionOnBoard board BlinkySpawn
    inkyPosition = findFieldPositionOnBoard board InkySpawn
    clydePosition = findFieldPositionOnBoard board ClydeSpawn
    pinkyPosition = findFieldPositionOnBoard board PinkySpawn
    blinkyHome = findFieldPositionOnBoard board BlinkyHome
    inkyHome = findFieldPositionOnBoard board InkyHome
    clydeHome = findFieldPositionOnBoard board ClydeHome
    pinkyHome = findFieldPositionOnBoard board PinkyHome
    spawnPosition = findFieldPositionOnBoard board Spawn

-- |Creating the initial Player record object
initialPlayer :: [Picture] -> Point -> Player
initialPlayer pics spawnLocation = Player { playerSprites = pics,
                                            currentPlayerSprite = pics !! 6,
                                            playerPosition = spawnLocation,
                                            playerAnimationState = Open,
                                            playerDirection = None,
                                            playerFutureDirection = None,
                                            playerState = PlayerAlive,
                                            playerStateTimer = 0,
                                            elapsedPlayerFrames = 0,
                                            playerXSteps = 0,
                                            playerYSteps = 0,
                                            lives = 3
                                          }

-- |Creating an initial ghost record object
initialGhost :: [Picture] -> (Float, Float) -> (Float, Float) -> GhostName -> Int -> Ghost
initialGhost pics spawn home name releaseTime = Ghost {   ghostSprites = pics,
                                                          ghostName = name,
                                                          ghostPosition = spawn,
                                                          spawn = spawn,
                                                          home = home,
                                                          ghostDirection = None,
                                                          ghostState = Trapped,
                                                          ghostXSteps = 0,
                                                          ghostYSteps = 0,
                                                          targetPosition = (0, 0),
                                                          releaseTime = releaseTime
                                                      }

-- |Create the board from the map textfile
createBoard :: String -> Board
createBoard t = map (rowToFields . words) (lines t)

-- |Map a string to the corresponding Field
rowToFields :: [String] -> Row
rowToFields = map stringToField
  where stringToField :: String -> Field
        stringToField "p" = Pacdot
        stringToField "pp" = PowerPacdot

        stringToField "uc" = UpCorner
        stringToField "dc" = DownCorner
        stringToField "lc" = LeftCorner
        stringToField "rc" = RightCorner

        stringToField "l" = Horizontal
        stringToField "v" = Vertical

        stringToField "uo" = UpConnector
        stringToField "do" = DownConnector
        stringToField "lo" = LeftConnector
        stringToField "ro" = RightConnector
        stringToField "ao" = AllConnector

        stringToField "tr" = TopRounded
        stringToField "br" = BottomRounded
        stringToField "rr" = RightRounded
        stringToField "lr" = LeftRounded

        stringToField "g1" = BlinkySpawn
        stringToField "g2" = InkySpawn
        stringToField "g3" = ClydeSpawn
        stringToField "g4" = PinkySpawn

        -- Will be drawn as pacdots later, for now this visualizes things
        stringToField "h1" = BlinkyHome
        stringToField "h2" = InkyHome
        stringToField "h3" = ClydeHome
        stringToField "h4" = PinkyHome

        -- Will be drawn as pacdots later, for now this visualizes things
        stringToField "t" = Transporter

        stringToField "f" = Fruit

        stringToField "e" = Empty

        stringToField "s" = Spawn

        stringToField "E" = OutOfBounds

        stringToField _ = Empty

-- |Calculate the size a single square in our grid should be to make sure the map is as large as possible while not being larger than the viewport
calculateGridSize :: Float -> Float -> Float
calculateGridSize numberOfColumns numberOfRows  | 800 / numberOfColumns < 720 / numberOfRows = 800 / numberOfColumns
                                                | otherwise = 720 / numberOfRows

--------------------------------------------------
-------------------DATA TYPES---------------------
--------------------------------------------------

-- |The gamestate record object
data GameState = GameState {  gameState       :: State,
                              player          :: Player,
                              board           :: Board,
                              blinky          :: Ghost,
                              inky            :: Ghost,
                              pinky           :: Ghost,
                              clyde           :: Ghost,
                              score           :: Int,
                              multiplier      :: Int,
                              elapsedTime     :: Float,
                              elapsedBoardFrames   :: Int,
                              numberOfRows    :: Float,
                              numberOfColumns :: Float,
                              gridSize        :: Float,
                              pics            :: [Picture],
                              stdGen          :: StdGen,
                              pacDotsOnBoard  :: Int,
                              spawnLocation   :: Point,        -- The location pacman spawns and the location to which the ghosts will be freed
                              scatterTimer    :: Float,
                              ghostStates     :: GhostState,
                              ghostSpritesS   :: [[Picture]],
                              highScores      :: [String]
                          }

-- |The player record object
data Player = Player {  playerPosition        :: Point,
                        playerState           :: PlayerState,
                        playerStateTimer      :: Float,
                        playerAnimationState  :: PlayerAnimationState,
                        playerDirection       :: MovementDirection,
                        playerFutureDirection :: MovementDirection,
                        currentPlayerSprite   :: Picture,
                        playerSprites         :: [Picture],
                        elapsedPlayerFrames   :: Int,
                        playerXSteps          :: Int,
                        playerYSteps          :: Int,
                        lives                 :: Int
                    }

-- |The player animation state: Open, closed
data PlayerAnimationState = Open | Closed
  deriving (Eq)

-- |The player state: Alive, dead, boosted
data PlayerState = PlayerAlive | PlayerDead | PlayerBoosted
  deriving (Eq, Show)

-- |The ghost record object
data Ghost  = Ghost { ghostName       :: GhostName,   -- Weet niet hoe er anders gepattern matched kan worden op welke ghost er wordt meegegeven aan de update functie van het ghost object?
                      ghostPosition   :: Point,
                      ghostState      :: GhostState,
                      home            :: Point,
                      spawn           :: Point,
                      ghostDirection  :: MovementDirection,
                      ghostSprites    :: [Picture],
                      ghostXSteps     :: Int,
                      ghostYSteps     :: Int,
                      targetPosition  :: Point,
                      releaseTime     :: Int
                    }

-- |The state of the game: Playing, game over, paused
data State = Playing | GameOver | Paused
  deriving (Eq)

-- |The state of a ghost: Chasing, scared, dead, scattering, trapped or unfrightenable
data GhostState = Chasing | Scared | Dead | Scattering | Trapped | Unfrightenable
  deriving (Eq, Show)

data GhostName = Blinky | Inky | Clyde | Pinky
  deriving (Eq)

-- |The direction the ghost/player is moving or going to move: Up, down, left, right, none
data MovementDirection = Up | Down | Left | Right | None
  deriving (Eq, Show)

-- |A field on the board
data Field = Pacdot | Energizer | Fruit | Empty | RightCorner | DownCorner | LeftCorner | UpCorner 
            | Horizontal | Vertical | UpConnector | DownConnector 
            | LeftConnector | RightConnector | AllConnector | LeftRounded 
            | RightRounded | TopRounded | BottomRounded
            | BlinkySpawn | InkySpawn | ClydeSpawn | PinkySpawn
            | BlinkyHome  | InkyHome  | ClydeHome  | PinkyHome
            | Transporter | PowerPacdot | Spawn | OutOfBounds
  deriving (Eq, Show)
type Row = [Field]
type Board = [Row]

-- |Hitbox data type, contains the top left point of the hitbox. All hitboxes are 1x1 in size so width and height are not needed.
newtype Hitbox = Hitbox Point

--------------------------------------------------
-----------------TYPE CLASSES---------------------
--------------------------------------------------

{-|
  Direction typeclass:
  All items which have a movementDirection should implement this.
  Gives access to the 'direction' and 'updateMovementDirection' functions.
-}
class HasDirection a where
  -- |Get the current movement direction
  direction               :: a -> MovementDirection
  -- |Update the current movement direction
  updateMovementDirection :: GameState -> MovementDirection -> a -> a

{-|
  Position typeclass:
  All items which have a position should implement this.
  Gives access to the 'position' and 'stepsTaken' functions.
-}
class HasPosition a where
  -- |Get the current position
  position    :: a -> Point
  -- |Get the steps taken in x and y directions. In the form (xSteps, ySteps)
  stepsTaken  :: a -> (Int, Int)
  xSteps :: a -> Int
  ySteps :: a -> Int


{-|
  Renderable typeclass
  All items which can be rendered should implement this.
  Gives access to the 'render' function.
-}
class Renderable a where
  -- |Get a picture that can be rendered. Needs the gamestate, the object to render and the position to render the picture at
  render :: GameState -> a -> (Float, Float) -> Picture

{-|
  AnimationState typeclass
  Any animation has animation states. Instancing this typeclass gives us an easy way to go to the next frame of the animation.
-}
class AnimationState a where
  -- |Get the next animation state from this animation state
  nextState :: a -> a

{-|
  Animatable typeclass
  All items which can be animated should implement this.
  Gives access to the 'elapsedFrames' function.
-}
class Animatable a where
  -- |Get the number of frames that have elapsed for this object
  elapsedFrames :: a -> Int

{-|
  Updateable typeclass:
  Everything that can be updated in the 'step' function should implement this.
  Gives access to the 'update' function which takes a Gamestate and returns an updated version of the object.
-}
class Updateable a where
  update :: GameState -> a -> a

{-|
  Collidable typeclass:
  Everything that can collide should implement this.
  Gives access to the 'hitbox' function which returns the hitbox of the object. This hitbox can be used to determine collision of two objects.
-}
class Collidable a where
  hitbox :: a -> Hitbox

{-|
  Renderable instances
  Board renderable instance
-}
instance Renderable Field where
  render gstate Horizontal = scaleAndTranslate gstate (head $ pics gstate)
  render gstate Vertical = scaleAndTranslate gstate (pics gstate !! 1)

  render gstate RightCorner = scaleAndTranslate gstate (pics gstate !! 2)
  render gstate DownCorner = scaleAndTranslate gstate (pics gstate !! 3)
  render gstate LeftCorner = scaleAndTranslate gstate (pics gstate !! 4)
  render gstate UpCorner = scaleAndTranslate gstate (pics gstate !! 5)

  render gstate UpConnector = scaleAndTranslate gstate (pics gstate !! 6)
  render gstate DownConnector = scaleAndTranslate gstate (pics gstate !! 7)
  render gstate LeftConnector = scaleAndTranslate gstate (pics gstate !! 8)
  render gstate RightConnector = scaleAndTranslate gstate (pics gstate !! 9)
  render gstate AllConnector = scaleAndTranslate gstate (pics gstate !! 10)

  render gstate TopRounded = scaleAndTranslate gstate (pics gstate !! 11)
  render gstate BottomRounded = scaleAndTranslate gstate (pics gstate !! 12)
  render gstate RightRounded = scaleAndTranslate gstate (pics gstate !! 13)
  render gstate LeftRounded = scaleAndTranslate gstate (pics gstate !! 14)

  render gstate Empty = translatePicture gstate (color black . rectangleSolid 10 $ 10)

  render gstate Fruit = scaleAndTranslate gstate (pics gstate !! 15)

  render gstate BlinkyHome = translatePicture gstate (color white . circleSolid $ 10)
  render gstate InkyHome = translatePicture gstate (color white . circleSolid $ 10)
  render gstate ClydeHome = translatePicture gstate (color white . circleSolid $ 10)
  render gstate PinkyHome = translatePicture gstate (color white . circleSolid $ 10)

  render gstate Transporter = translatePicture gstate (color blue . circleSolid $ 5)

  render gstate Pacdot = translatePicture gstate (color white . circleSolid $ 5)
  render gstate PowerPacdot = translatePicture gstate (color white . circleSolid $ 10)

  render gstate Spawn = translatePicture gstate (color red . circleSolid $ 5)

  render gstate _ = translatePicture gstate (color black . circleSolid $ 1)

--------------------------------------------------
----------------HELPER FUNCTIONS------------------
--------------------------------------------------

-- |Function to scale a picture to a certain size (all our bitmaps are 100x100 thus the gridSize should always be divided by 100)
scalePicture :: GameState -> Picture -> Picture
scalePicture gstate = scale (gSize / 100) (gSize / 100)
  where gSize = gridSize gstate

-- |Function to translate a picture to a location on the screen at a certain x and y index in the grid world
translatePicture :: GameState -> Picture -> (Float, Float) -> Picture
translatePicture gstate p (x, y) = Translate ((-(nColumns * gSize) * 0.5) + 0.5 * gSize + x * gSize) (((nRows * gSize) * 0.5) - 0.5 * gSize - y * gSize) p
  where gSize = gridSize gstate
        nColumns  = numberOfColumns gstate
        nRows     = numberOfRows gstate

-- |Scale and translate an image. Takes a gamestate, picture and the location you would like to translate the picture to. Pictures are automatically scaled to grid size.
scaleAndTranslate :: GameState -> Picture -> (Float, Float) -> Picture
scaleAndTranslate gstate = translatePicture gstate . scalePicture gstate

-- |Helper function to find the "x, y" position of a field on the board
findFieldPositionOnBoard :: Board -> Field -> (Float, Float)
findFieldPositionOnBoard board field = (findFieldIndex . filter p) zippedRows
  where zippedRows = zip [0 ..] board

        p :: (Int, [Field]) -> Bool
        p (yIndex, row) = field `elem` row

        findFieldIndex :: [(Int, Row)] -> (Float, Float)
        findFieldIndex [(y, row)] = (fromIntegral . fromJust $ elemIndex field row, fromIntegral y)

-- |Returns True if x is an int to n decimal places
isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = round (10^fromIntegral n * x - fromIntegral (round x)) == 0

-- |Returns the type of field at a certain location on the board
fieldAtPosition :: Board -> Int -> (Float, Float) -> Field
fieldAtPosition board numberOfColumns (x, y) = concat board !! (numberOfColumns * round y + round x)

-- |Returns the field in a certain direction
fieldAtFuturePosition :: (Float, Float) -> Board -> MovementDirection -> Int -> Field
fieldAtFuturePosition (x, y) board md numberOfColumns = concat board !! (numberOfColumns * roundY md y + roundX md x)

-- |Return a rounding method to use depending on the direction we want to travel
roundY :: MovementDirection -> (Float -> Int)
roundY Model.Up     = ceiling
roundY Model.Down   = floor
roundY Model.Left   = round
roundY Model.Right  = round
roundY _            = round

-- |Return a rounding method to use depending on the direction we want to travel
roundX :: MovementDirection -> (Float -> Int)
roundX Model.Up    = round
roundX Model.Down  = round
roundX Model.Left  = floor
roundX Model.Right = ceiling
roundX _           = round

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

-- |Check if there is a Pacdot or Empty field in the new direction
checkFieldInFuturePosition :: (Field -> Bool) -> MovementDirection -> Board -> Int -> (Float, Float) -> Bool
checkFieldInFuturePosition _ Model.None  _     _                        _     = False
checkFieldInFuturePosition f Model.Up    board numberOfColumns pos = (f . fieldAtFuturePosition pos board Model.Up) numberOfColumns
checkFieldInFuturePosition f Model.Down  board numberOfColumns pos = (f . fieldAtFuturePosition pos board Model.Down) numberOfColumns
checkFieldInFuturePosition f Model.Left  board numberOfColumns pos = (f . fieldAtFuturePosition pos board Model.Left) numberOfColumns
checkFieldInFuturePosition f Model.Right board numberOfColumns pos = (f . fieldAtFuturePosition pos board Model.Right) numberOfColumns

-- |Helper function for the 'isFieldEmptyOrPacdot' function, seperate for fruit bc fruit cannot spawn on fruit
emptyOrPacdotHelperFruit :: Field -> Bool
emptyOrPacdotHelperFruit field = field ==  Empty  || field == Pacdot
                                                  || field == Spawn

emptyOrPacdotHelper :: Field -> Bool
emptyOrPacdotHelper field = field ==  Empty || field == Pacdot 
                                            || field == BlinkyHome 
                                            || field == InkyHome 
                                            || field == ClydeHome 
                                            || field == PinkyHome
                                            || field == PowerPacdot
                                            || field == Spawn
                                            || field == Fruit

-- |Count how many pacdots there are on the board
numberOfPacdotsOnTheBoard :: Board -> Int
numberOfPacdotsOnTheBoard = sum . map (length . filter p)
  where
    p :: Field -> Bool
    p = isPacdot

-- |Helper function which takes a field and returns a boolean denoting whether it is a pacdot or equivalent field
isPacdot :: Field -> Bool
isPacdot f = f == Pacdot || f == Spawn

isFruit :: Field -> Bool
isFruit f = f == Fruit 

-- |Helper function which takes a field and returns a boolean denoting whether it is a transporter field
isTransporter :: Field -> Bool
isTransporter f = f == Transporter

-- |Helper function which takes a field and returns a boolean denoting whether it is a power pacdot field
isPowerPacdot :: Field -> Bool
isPowerPacdot f = f == PowerPacdot  || f == BlinkyHome
                                    || f == InkyHome
                                    || f == ClydeHome
                                    || f == PinkyHome

-- |Eat the pacdot at a position
eatPacdot :: Board -> Point -> Board
eatPacdot board = changeFieldAtPosition board Empty

-- |Takes the board, a field and a position. Changes the field at the given position with the new field
changeFieldAtPosition :: Board -> Field -> Point -> Board
changeFieldAtPosition board field (x, y) = board & element (round y) . element (round x) .~ field

-- |Get a point a set distance in the movement direction
pointAtDistanceInMovementDirection :: Point -> MovementDirection -> Float -> Point
pointAtDistanceInMovementDirection pos    Model.None  _         = pos
pointAtDistanceInMovementDirection (x, y) Model.Up    distance  = (x, y + distance)
pointAtDistanceInMovementDirection (x, y) Model.Down  distance  = (x, y - distance)
pointAtDistanceInMovementDirection (x, y) Model.Left  distance  = (x - distance, y)
pointAtDistanceInMovementDirection (x, y) Model.Right distance  = (x + distance, y)

--------------------------------------------------
----------------POINTS AND VECTORS----------------
--------------------------------------------------

-- |Calculate distance between two points, not squared to reduce strain on system. Higher number = higher distance
distanceBetweenTwoPoints :: Point -> Point -> Float
distanceBetweenTwoPoints (x1, y1) (x2, y2) = (x2 - x1) ^ 2 + (y2 - y1) ^ 2  

-- |Create a vector between two points
createVector :: Point -> Point -> Vector
createVector (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- |Scalar multiplication with vector
(<*>) :: Float -> Vector -> Vector
s <*> (x, y) = (s * x, s * y)

--------------------------------------------------
--------------------COLLISIONS--------------------
--------------------------------------------------

-- |Determine whether there is a collision between two Collidable objects
collision :: (Collidable a, Collidable b) => a -> b -> Bool
collision a b = hitBoxesOverlapping (hitbox a) (hitbox b)
  where
    hitBoxesOverlapping :: Hitbox -> Hitbox -> Bool
    hitBoxesOverlapping (Hitbox (x1, y1)) (Hitbox (x2, y2)) = x1 < x2 + 1
                                                            && x1 + 1 > x2
                                                            && y1 < y2 + 1
                                                            && y1 + 1 > y2