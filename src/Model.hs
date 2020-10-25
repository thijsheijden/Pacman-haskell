-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random
import Graphics.Gloss
import Data.Maybe
import Data.List

-- Creating the initial gamestate record object
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
                                                          blinky = initialGhost (head ghostSprites) blinkyPosition blinkyHome,
                                                          inky = initialGhost (ghostSprites !! 1) inkyPosition inkyHome,
                                                          clyde = initialGhost (ghostSprites !! 2) clydePosition clydeHome,
                                                          pinky = initialGhost (ghostSprites !! 3) pinkyPosition pinkyHome
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
    blinkyHome = findFieldPositionOnBoard board BlinkyHome
    inkyHome = findFieldPositionOnBoard board InkyHome
    clydeHome = findFieldPositionOnBoard board ClydeHome
    pinkyHome = findFieldPositionOnBoard board PinkyHome

-- Creating the initial Player record object
initialPlayer :: [Picture] -> Player
initialPlayer pics = Player { playerSprites = pics,
                              currentPlayerSprite = pics !! 6,
                              playerPosition = (10, 8),
                              animationState = Open,
                              playerDirection = None,
                              playerFutureDirection = None,
                              playerState = PlayerAlive,
                              elapsedPlayerFrames = 0
                            }

-- Creating an initial ghost record object
initialGhost :: [Picture] -> (Float, Float) -> (Float, Float) -> Ghost
initialGhost pics spawn home = Ghost {  ghostSprites = pics,
                                        ghostPosition = spawn,
                                        home = home,
                                        ghostDirection = None,
                                        ghostState = Trapped,
                                        speed = 1}

-- The gamestate record object
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

-- The player record object
data Player = Player {  playerPosition  :: Point,
                        playerState     :: PlayerState,
                        animationState  :: PlayerAnimationState,
                        playerDirection       :: MovementDirection,
                        playerFutureDirection :: MovementDirection,
                        currentPlayerSprite   :: Picture,
                        playerSprites    :: [Picture],
                        elapsedPlayerFrames   :: Int
                    }

data PlayerAnimationState = Open | Closed

data PlayerState = PlayerAlive | PlayerDead | PlayerBoosted

-- The ghost record object
data Ghost  = Ghost { ghostPosition   :: Point,
                      ghostState      :: GhostState,
                      home            :: Point,
                      speed           :: Float,
                      ghostDirection  :: MovementDirection,
                      ghostSprites    :: [Picture]
                    }

-- The state of the game
data State = Playing | GameOver | Paused

-- The state of a ghost
data GhostState = Chasing | Scared | Dead | Scattering | Trapped

-- The direction the ghost/player is moving or going to move
data MovementDirection = Up | Down | Left | Right | None
  deriving (Eq)

-- A field on the board
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

-- Create the board from the map textfile
createBoard :: String -> Board
createBoard t = map (rowToFields . words) (lines t)

-- Map a string to the corresponding Field
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

-- Calculate the size a single square in our grid should be to make sure the map is as large as possible while not being larger than the viewport
calculateGridSize :: Board -> Float -> Float -> Float
calculateGridSize board numberOfColumns numberOfRows  | 1280 / numberOfColumns < 720 / numberOfRows = 1280 / numberOfColumns
                                                      | otherwise = 720 / numberOfRows

-- Direction typeclass
-- All items which have a movementDirection should implement this
class HasDirection a where
  direction :: a -> MovementDirection
  updateMovementDirection :: MovementDirection -> a -> a

-- Position typeclass
-- All items which have a position should implement this
class HasPosition a where
  position :: a -> Point

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

  render gstate Empty = translatePicture gstate (color black . rectangleSolid 10 $ 10)

  render gstate Cherry = scaleAndTranslate gstate (pics gstate !! 15)

  render gstate BlinkyHome = translatePicture gstate (color red . circleSolid $ 5)
  render gstate InkyHome = translatePicture gstate (color (light blue) . circleSolid $ 5)
  render gstate ClydeHome = translatePicture gstate (color cyan . circleSolid $ 5)
  render gstate PinkyHome = translatePicture gstate (color magenta . circleSolid $ 5)

  render gstate Transporter = translatePicture gstate (color blue . circleSolid $ 5)

  render gstate _ = translatePicture gstate (color white . circleSolid $ 5)

-- Function to scale a picture to a certain size (all our bitmaps are 100x100 thus the gridSize should always be divided by 100)
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
