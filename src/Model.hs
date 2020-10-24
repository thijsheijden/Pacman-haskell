-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random
import Graphics.Gloss

initialState :: StdGen -> String -> [Picture] -> GameState
initialState stdGen map pics = GameState { stdGen = stdGen, board = createBoard map, pics = pics }

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
            | RightRounder | TopRounded | BottomRounded
type Row = [Field]
type Board = [Row]

-- Create the board from the map textfile
createBoard :: String -> Board
createBoard t = map (rowToFields . words) (lines t)

rowToFields :: [String] -> Row
rowToFields = map stringToField
  where stringToField :: String -> Field
        stringToField "b" = Pacdot
        stringToField "uc" = UpCorner
        stringToField "dc" = DownCorner
        stringToField "lc" = LeftCorner
        stringToField "rc" = RightCorner
        stringToField "l" = Horizontal
        stringToField "v" = Vertical

-- Renderable typeclass
-- All items which can be rendered should implement this
class Renderable a where
  render :: GameState -> a -> Float -> Float -> Picture

instance Renderable Field where
  render gstate Pacdot = translatePicture gstate (circleSolid (gridSize gstate))
  render gstate Horizontal = translatePicture gstate (head $ pics gstate)
  render gstate Vertical = translatePicture gstate (pics gstate !! 1)
  render gstate _ = translatePicture gstate (pics gstate !! 1)
  -- render gstate RightCorner = translatePicture gstate (pics gstate !! 2)
  -- render gstate DownCorner = translatePicture gstate (pics gstate !! 3)
  -- render gstate LeftCorner = translatePicture gstate (pics gstate !! 4)
  -- render gstate UpCorner = translatePicture gstate (pics gstate !! 5)


-- Function to translate a picture to a location on the screen from x,y position
translatePicture :: GameState -> Picture -> Float -> Float -> Picture
translatePicture gstate p x y = Translate ((-(nColumns * gSize) * 0.5) + 0.5 * gSize + x * gSize) (((nRows * gSize) * 0.5) - 0.5 * gSize - y * gSize) p
  where gSize = gridSize gstate
        nColumns = numberOfColumns gstate
        nRows = numberOfRows gstate

