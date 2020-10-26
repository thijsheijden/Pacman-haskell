-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random
import Graphics.Gloss
import Data.Maybe
import Data.List

secsBetweenCycles :: Float
secsBetweenCycles = 4

--------------------------------------------------
---------------INITIAL STATES---------------------
--------------------------------------------------

-- |Creating the initial gamestate record object
initialState :: StdGen -> String -> [Picture] -> [Picture] -> [[Picture]] -> GameState
initialState stdGen map pics playerSprites ghostSprites = GameState {  player = initialPlayer playerSprites,
                                                          stdGen = stdGen,
                                                          score = 0,
                                                          gameState = Playing,
                                                          board = board, 
                                                          pics = pics, 
                                                          gridSize = gridSize, 
                                                          numberOfColumns = numberOfColumns, 
                                                          numberOfRows = numberOfRows,
                                                          multiplier = 1,
                                                          elapsedTime = 0,
                                                          elapsedBoardFrames = 0,
                                                          blinky = initialGhost (head ghostSprites) blinkyPosition blinkyHome,
                                                          inky = initialGhost (ghostSprites !! 1) inkyPosition inkyHome,
                                                          clyde = initialGhost (ghostSprites !! 2) clydePosition clydeHome,
                                                          pinky = initialGhost (ghostSprites !! 3) pinkyPosition pinkyHome
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

-- |Creating the initial Player record object
initialPlayer :: [Picture] -> Player
initialPlayer pics = Player { playerSprites = pics,
                              currentPlayerSprite = pics !! 6,
                              playerPosition = (10, 8),
                              playerAnimationState = Open,
                              playerDirection = None,
                              playerFutureDirection = None,
                              playerState = PlayerAlive,
                              elapsedPlayerFrames = 0,
                              xSteps = 0,
                              ySteps = 0
                            }

-- |Creating an initial ghost record object
initialGhost :: [Picture] -> (Float, Float) -> (Float, Float) -> Ghost
initialGhost pics spawn home = Ghost {  ghostSprites = pics,
                                        ghostPosition = spawn,
                                        home = home,
                                        ghostDirection = None,
                                        ghostState = Trapped
                                    }

-- |Create the board from the map textfile
createBoard :: String -> Board
createBoard t = map (rowToFields . words) (lines t)

-- |Map a string to the corresponding Field
rowToFields :: [String] -> Row
rowToFields = map stringToField
  where stringToField :: String -> Field
        stringToField "p" = Pacdot

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

        stringToField "c" = Cherry

        stringToField "e" = Empty

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
                              stdGen          :: StdGen
                          }

-- |The player record object
data Player = Player {  playerPosition        :: Point,
                        playerState           :: PlayerState,
                        playerAnimationState  :: PlayerAnimationState,
                        playerDirection       :: MovementDirection,
                        playerFutureDirection :: MovementDirection,
                        currentPlayerSprite   :: Picture,
                        playerSprites         :: [Picture],
                        elapsedPlayerFrames   :: Int,
                        xSteps                :: Int,
                        ySteps                :: Int
                    }

-- |The player animation state: Open, closed
data PlayerAnimationState = Open | Closed
  deriving (Eq)

-- |The player state: Alive, dead, boosted
data PlayerState = PlayerAlive | PlayerDead | PlayerBoosted
  deriving (Eq)

-- |The ghost record object
data Ghost  = Ghost { ghostPosition   :: Point,
                      ghostState      :: GhostState,
                      home            :: Point,
                      ghostDirection  :: MovementDirection,
                      ghostSprites    :: [Picture]
                    }

-- |The state of the game: Playing, game over, paused
data State = Playing | GameOver | Paused
  deriving (Eq)

-- |The state of a ghost: Chasing, scared, dead, scattering, trapped
data GhostState = Chasing | Scared | Dead | Scattering | Trapped
  deriving (Eq)

-- |The direction the ghost/player is moving or going to move: Up, down, left, right, none
data MovementDirection = Up | Down | Left | Right | None
  deriving (Eq, Show)

-- |A field on the board
data Field = Pacdot | Energizer | Cherry | Empty | RightCorner | DownCorner | LeftCorner | UpCorner 
            | Horizontal | Vertical | UpConnector | DownConnector 
            | LeftConnector | RightConnector | AllConnector | LeftRounded 
            | RightRounded | TopRounded | BottomRounded
            | BlinkySpawn | InkySpawn | ClydeSpawn | PinkySpawn
            | BlinkyHome  | InkyHome  | ClydeHome  | PinkyHome
            | Transporter
  deriving (Eq)
type Row = [Field]
type Board = [Row]

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

  render gstate Cherry = scaleAndTranslate gstate (pics gstate !! 15)

  render gstate BlinkyHome = translatePicture gstate (color red . circleSolid $ 5)
  render gstate InkyHome = translatePicture gstate (color (light blue) . circleSolid $ 5)
  render gstate ClydeHome = translatePicture gstate (color cyan . circleSolid $ 5)
  render gstate PinkyHome = translatePicture gstate (color magenta . circleSolid $ 5)

  render gstate Transporter = translatePicture gstate (color blue . circleSolid $ 5)

  render gstate _ = translatePicture gstate (color white . circleSolid $ 5)

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
fieldAtPosition :: (Float, Float) -> Board -> Int -> Field
fieldAtPosition (x, y) board numberOfColumns = concat board !! (numberOfColumns * ceiling y + ceiling x)

-- |Returns the field in a certain direction
fieldAtFuturePosition :: (Float, Float) -> Board -> MovementDirection -> Int -> Field
fieldAtFuturePosition (x, y) board md numberOfColumns = concat board !! (numberOfColumns * roundY md y + roundX md x)
  where
    -- Return a rounding method to use depending on the direction we want to travel
    roundY :: MovementDirection -> (Float -> Int)
    roundY Model.Up     = ceiling
    roundY Model.Down   = floor
    roundY Model.Left   = round
    roundY Model.Right  = round
    roundY _            = round

    roundX :: MovementDirection -> (Float -> Int)
    roundX Model.Up    = round
    roundX Model.Down  = round
    roundX Model.Left  = floor
    roundX Model.Right = ceiling
    roundX _           = round

-- |Check if there is a Pacdot or Empty field in the new direction
isFieldEmptyOrPacdot :: MovementDirection -> Board -> Int -> Float -> (Float, Float) -> Bool
isFieldEmptyOrPacdot Model.None  _     _               _         _     = False
isFieldEmptyOrPacdot Model.Up    board numberOfColumns distance (x, y) = (emptyOrPacdotHelper . fieldAtFuturePosition (x, y + distance) board Model.Up) numberOfColumns
isFieldEmptyOrPacdot Model.Down  board numberOfColumns distance (x, y) = (emptyOrPacdotHelper . fieldAtFuturePosition (x, y - distance) board Model.Down) numberOfColumns
isFieldEmptyOrPacdot Model.Left  board numberOfColumns distance (x, y) = (emptyOrPacdotHelper . fieldAtFuturePosition (x - distance, y) board Model.Left) numberOfColumns
isFieldEmptyOrPacdot Model.Right board numberOfColumns distance (x, y) = (emptyOrPacdotHelper . fieldAtFuturePosition (x + distance, y) board Model.Right) numberOfColumns

-- |Helper function for the 'isFieldEmptyOrPacdot' function
emptyOrPacdotHelper :: Field -> Bool
emptyOrPacdotHelper field = field ==  Empty || field == Pacdot 
                                            || field == BlinkyHome 
                                            || field == InkyHome 
                                            || field == ClydeHome 
                                            || field == PinkyHome 