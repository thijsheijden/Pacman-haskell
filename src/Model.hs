-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random
import Graphics.Gloss
import Data.Maybe
import Data.List

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
                                                          elapsedFrames = 0,
                                                          blinky = initialGhost (head ghostSprites) blinkyPosition,
                                                          inky = initialGhost (ghostSprites !! 1) inkyPosition,
                                                          clyde = initialGhost (ghostSprites !! 2) clydePosition,
                                                          pinky = initialGhost (ghostSprites !! 3) pinkyPosition
                                                        }
  where
    board = createBoard map
    numberOfColumns = fromIntegral $ length $ head board
    numberOfRows = fromIntegral $ length board
    gridSize = calculateGridSize board numberOfColumns numberOfRows
    blinkyPosition = findFieldPositionOnBoard board BlinkySpawn
    inkyPosition = findFieldPositionOnBoard board InkySpawn
    clydePosition = findFieldPositionOnBoard board ClydeSpawn
    pinkyPosition = findFieldPositionOnBoard board PinkySpawn

-- Creating the initial Player record object
initialPlayer :: [Picture] -> Player
initialPlayer pics = Player { playerSprites = pics,
                              currentPlayerSprite = pics !! 6,
                              playerPosition = (10, 8),
                              animationState = Open,
                              playerDirection = None
                            }

-- Creating an initial ghost record object
initialGhost :: [Picture] -> (Float, Float) -> Ghost
initialGhost pics spawn = Ghost { ghostSprites = pics,
                            ghostPosition = spawn,
                            ghostDirection = None }

data GameState = GameState {  gameState     :: State,
                              player        :: Player,
                              board         :: Board,
                              blinky        :: Ghost,
                              inky          :: Ghost,
                              pinky         :: Ghost,
                              clyde         :: Ghost,
                              score         :: Int,
                              multiplier    :: Int,
                              elapsedTime   :: Float,
                              elapsedFrames :: Int,
                              numberOfRows  :: Float,
                              numberOfColumns :: Float,
                              gridSize      :: Float,
                              pics          :: [Picture],
                              stdGen        :: StdGen
                          }

data Player = Player {  playerPosition  :: Point,
                        playerState     :: PlayerState,
                        animationState  :: PlayerAnimationState,
                        playerDirection       :: MovementDirection,
                        playerFutureDirection :: MovementDirection,
                        currentPlayerSprite   :: Picture,
                        playerSprites    :: [Picture],
                        elapsedPlayerFrames   :: Int
                    }

data Ghost  = Ghost { ghostPosition   :: Point,
                      ghostState      :: GhostState,
                      home            :: Point,
                      speed           :: Float,
                      released        :: Bool,
                      ghostDirection  :: MovementDirection,
                      ghostSprites    :: [Picture]
                    }

data State = Playing | GameOver | Paused

data PlayerAnimationState = Open | Closed

data PlayerState = PlayerAlive | PlayerDead

data GhostState = Chasing | Scared | Dead | Scattering

data MovementDirection = Up | Down | Left | Right | None
  deriving (Eq)

data Field = Pacdot | Energizer | Cherry | Empty | RightCorner | DownCorner | LeftCorner | UpCorner 
            | Horizontal | Vertical | UpConnector | DownConnector 
            | LeftConnector | RightConnector | AllConnector | LeftRounded 
            | RightRounded | TopRounded | BottomRounded
            | BlinkySpawn | InkySpawn | ClydeSpawn | PinkySpawn
  deriving (Eq)
type Row = [Field]
type Board = [Row]

-- Create the board from the map textfile
createBoard :: String -> Board
createBoard t = map (rowToFields . words) (lines t)

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

        stringToField "c" = Cherry

        stringToField "e" = Empty

        stringToField _ = Empty


calculateGridSize :: Board -> Float -> Float -> Float
calculateGridSize board numberOfColumns numberOfRows  | 1280 / numberOfColumns < 720 / numberOfRows = 1280 / numberOfColumns
                                                      | otherwise = 720 / numberOfRows

-- Direction typeclass
-- All items which have a movementDirection should implement this
class HasDirection a where
  direction :: a -> MovementDirection
  updateMovementDirection :: MovementDirection -> a -> a

instance HasDirection Player where
  direction = playerDirection
  updateMovementDirection direction player = player { playerDirection = direction}

instance HasDirection Ghost where
  direction = ghostDirection
  updateMovementDirection direction ghost = ghost { ghostDirection = direction }

-- Position typeclass
-- All items which have a position should implement this
class HasPosition a where
  position :: a -> Point

instance HasPosition Player where
  position = playerPosition

instance HasPosition Ghost where
  position = ghostPosition

-- Renderable typeclass
-- All items which can be rendered should implement this
class Renderable a where
  render :: GameState -> a -> (Float, Float) -> Picture

-- Renderable instances
-- Board renderable instance
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

  render gstate Empty = translatePicture gstate (color black $ rectangleSolid 10 10)

  render gstate Cherry = scaleAndTranslate gstate (pics gstate !! 15)

  render gstate _ = translatePicture gstate (color white $ circleSolid 5)

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

-- Ghost renderable instance
instance Renderable Ghost where
  render gstate ghost = scaleAndTranslate gstate (currentGhostSprite ghost)
    where currentGhostSprite :: Ghost -> Picture
          currentGhostSprite ghost  | direction ghost == Model.None = head $ ghostSprites ghost
                                    | direction ghost == Model.Up = head $ ghostSprites ghost
                                    | direction ghost == Model.Down = ghostSprites ghost !! 1
                                    | direction ghost == Model.Left = ghostSprites ghost !! 2
                                    | direction ghost == Model.Right = ghostSprites ghost !! 3


-- Function to scale a picture to a certain size
scalePicture :: GameState -> Picture -> Picture
scalePicture gstate = scale (gSize / 100) (gSize / 100)
  where gSize = gridSize gstate

-- Function to translate a picture to a location on the screen at a certain x and y index in the grid world
translatePicture :: GameState -> Picture -> (Float, Float) -> Picture
translatePicture gstate p (x, y) = Translate ((-(nColumns * gSize) * 0.5) + 0.5 * gSize + x * gSize) (((nRows * gSize) * 0.5) - 0.5 * gSize - y * gSize) p
  where gSize = gridSize gstate
        nColumns = numberOfColumns gstate
        nRows = numberOfRows gstate

scaleAndTranslate :: GameState -> Picture -> (Float, Float) -> Picture
scaleAndTranslate gstate = translatePicture gstate . scalePicture gstate

-- Helper function to find the "x, y" position of a field on the board
findFieldPositionOnBoard :: Board -> Field -> (Float, Float)
findFieldPositionOnBoard board field = (findFieldIndex . filter p) zippedRows
  where zippedRows = zip [0 ..] board

        p :: (Int, [Field]) -> Bool
        p (yIndex, row) = field `elem` row

        findFieldIndex :: [(Int, Row)] -> (Float, Float)
        findFieldIndex [(y, row)] = (fromIntegral . fromJust $ elemIndex field row, fromIntegral y)
