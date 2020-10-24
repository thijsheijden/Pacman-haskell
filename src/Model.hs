-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random
import Graphics.Gloss

initialState :: StdGen -> String -> [Picture] -> GameState
initialState stdGen map pics = GameState {  stdGen = stdGen, 
                                            board = board, 
                                            pics = pics, 
                                            gridSize = gridSize, 
                                            numberOfColumns = numberOfColumns, 
                                            numberOfRows = numberOfRows
                                          }
  where
    board = createBoard map
    numberOfColumns = fromIntegral $ length $ head board
    numberOfRows = fromIntegral $ length board
    gridSize = calculateGridSize board numberOfColumns numberOfRows


data GameState = GameState {  player        :: Player,
                              board         :: Board,
                              blinky        :: Ghost,
                              inky          :: Ghost,
                              pinky         :: Ghost,
                              clyde         :: Ghost,
                              score         :: Int,
                              gameOver      :: Bool, 
                              paused        :: Bool,
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
                        direction       :: MovementDirection,
                        futureDirection :: MovementDirection
                    }

data Ghost  = Ghost { ghostPosition   :: Point,
                      ghostState      :: GhostState,
                      home            :: Point,
                      speed           :: Float,
                      released        :: Bool,
                      ghostDirection  :: MovementDirection
                    }

data PlayerState = PlayerAlive | PlayerDead

data GhostState = Chasing | Scared | Dead | Scattering

data MovementDirection = Up | Down | Left | Right | None

data Field = Pacdot | Energizer | Cherry | Empty | RightCorner | DownCorner | LeftCorner | UpCorner 
            | Horizontal | Vertical | UpConnector | DownConnector 
            | LeftConnector | RightConnector | AllConnector | LeftRounded 
            | RightRounded | TopRounded | BottomRounded
type Row = [Field]
type Board = [Row]

-- Create the board from the map textfile
createBoard :: String -> Board
createBoard t = map (rowToFields . words) (lines t)

rowToFields :: [String] -> Row
rowToFields = map stringToField
  where stringToField :: String -> Field
        stringToField "b" = Empty
        stringToField "uc" = UpCorner
        stringToField "dc" = DownCorner
        stringToField "lc" = LeftCorner
        stringToField "rc" = RightCorner
        stringToField "l" = Horizontal
        stringToField "v" = Vertical
        stringToField "uo" = UpConnector
        stringToField "tr" = TopRounded
        stringToField "br" = BottomRounded
        stringToField "rr" = RightRounded
        stringToField "lr" = LeftRounded

calculateGridSize :: Board -> Float -> Float -> Float
calculateGridSize board numberOfColumns numberOfRows  | 1280 / numberOfColumns < 720 / numberOfRows = 1280 / numberOfColumns
                                                      | otherwise = 720 / numberOfRows

-- Renderable typeclass
-- All items which can be rendered should implement this
class Renderable a where
  render :: GameState -> a -> Float -> Float -> Picture

instance Renderable Field where
  render gstate Horizontal = translatePicture gstate (scalePicture gstate (head $ pics gstate))
  render gstate Vertical = translatePicture gstate (scalePicture gstate (pics gstate !! 1))
  render gstate RightCorner = translatePicture gstate (scalePicture gstate (pics gstate !! 2))
  render gstate DownCorner = translatePicture gstate (scalePicture gstate (pics gstate !! 3))
  render gstate LeftCorner = translatePicture gstate (scalePicture gstate (pics gstate !! 4))
  render gstate UpCorner = translatePicture gstate (scalePicture gstate (pics gstate !! 5))
  render gstate UpConnector = translatePicture gstate (scalePicture gstate (pics gstate !! 6))
  render gstate TopRounded = translatePicture gstate (scalePicture gstate (pics gstate !! 7))
  render gstate BottomRounded = translatePicture gstate (scalePicture gstate (pics gstate !! 8))
  render gstate RightRounded = translatePicture gstate (scalePicture gstate (pics gstate !! 9))
  render gstate LeftRounded = translatePicture gstate (scalePicture gstate (pics gstate !! 10))
  render gstate Empty = translatePicture gstate (color white $ rectangleSolid 10 10)
  render gstate _ = translatePicture gstate (color white $ rectangleSolid (gridSize gstate) (gridSize gstate))


-- Function to scale a picture to a certain size
scalePicture :: GameState -> Picture -> Picture
scalePicture gstate = scale (gSize / 100) (gSize / 100)
  where gSize = gridSize gstate

-- Function to translate a picture to a location on the screen at a certain x and y index in the grid world
translatePicture :: GameState -> Picture -> Float -> Float -> Picture
translatePicture gstate p x y = Translate ((-(nColumns * gSize) * 0.5) + 0.5 * gSize + x * gSize) (((nRows * gSize) * 0.5) - 0.5 * gSize - y * gSize) p
  where gSize = gridSize gstate
        nColumns = numberOfColumns gstate
        nRows = numberOfRows gstate

