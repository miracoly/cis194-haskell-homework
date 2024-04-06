{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}

module Week2.Week2 (app) where

import CodeWorld
import Data.Text (Text)

app :: IO ()
app = exercise3

wall :: Picture
wall = colored black (rectangle 1 1) & colored color $ solidRectangle 1 1
 where
  color = HSL 0.50 0.47 0.63

ground :: Picture
ground = colored color $ solidRectangle 1 1
 where
  color = HSL 0.50 0.21 0.87

storage :: Picture
storage = colored colDot (solidCircle 0.2) & ground
 where
  colDot = translucent red

box :: Picture
box =
  colored colLine (polyline [(-0.35, 0.35), (0.35, -0.35)])
    & colored colLine (polyline [(-0.35, -0.35), (0.35, 0.35)])
    & colored colLine (rectangle 1 1)
    & colored colLine (rectangle 0.7 0.7)
    & colored colBox (solidRectangle 1 1)
 where
  colLine = black
  colBox = HSL 0.6 0.60 0.40

player :: Direction -> Picture
player d =
  case d of
    R ->
      playerBase
        & colored black (translated 0.05 0.22 (circle 0.01))
        & colored black (polyline [(0, 0.35), (-0.15, 0.2)])
    _ ->
      playerBase
        & colored black (translated (-0.05) 0.22 (circle 0.01))
        & colored black (polyline [(0, 0.35), (0.15, 0.2)])
 where
  playerBase :: Picture
  playerBase =
    colored black
      $ translated 0 0.2
      $ circle 0.15
      & colored black (polyline [(0, -0.15), (0, -0.4)])
      & colored black (polyline [(0, -0.4), (-0.15, -0.6)])
      & colored black (polyline [(0, -0.4), (0.15, -0.6)])
      & colored black (polyline [(0, -0.2), (0.15, -0.35)])
      & colored black (polyline [(0, -0.2), (-0.15, -0.35)])
      & colored black (polyline [(0, -0.2), (-0.15, -0.35)])

data Tile = Wall | Ground | Storage | Box | Blank

data Direction = R | U | L | D

data Coord = Coord Int Int

drawTile :: Tile -> Picture
drawTile t =
  case t of
    Wall -> wall
    Ground -> ground
    Storage -> storage
    Box -> box
    Blank -> blank

atCoord :: Coord -> Picture -> Picture
atCoord (Coord x y) = translated (fromIntegral x) (fromIntegral y)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord dir (Coord x y) =
  case dir of
    R -> Coord (x + 1) y
    U -> Coord x (y + 1)
    L -> Coord (x - 1) y
    D -> Coord x (y - 1)

someCoord :: Coord
someCoord = adjacentCoord U (adjacentCoord U (adjacentCoord L (Coord 100 0)))

pictureOfMaze :: Picture
pictureOfMaze = foldr ((&) . f) blank coords
 where
  f :: Coord -> Picture
  f c = atCoord c $ drawTile $ maze c
  coords :: [Coord]
  coords = [Coord x y | x <- [-10 .. 10], y <- [-10 .. 10]]

handleEvent :: Event -> State -> State
handleEvent (KeyPress k) oldState@(State _ c) =
  if isPlayerCoord (_stateCoord newState)
    then newState
    else oldState
 where
  newState = handleKey k
  handleKey :: Text -> State
  handleKey key =
    case key of
      "Right" -> State R $ adjacentCoord R c
      "Up" -> State U $ adjacentCoord U c
      "Left" -> State L $ adjacentCoord L c
      "Down" -> State D $ adjacentCoord D c
      _ -> oldState
handleEvent _ s = s

isPlayerCoord :: Coord -> Bool
isPlayerCoord = isCorrectTile . maze
 where
  isCorrectTile :: Tile -> Bool
  isCorrectTile t =
    case t of
      Ground -> True
      Storage -> True
      _ -> False

drawState :: State -> Picture
drawState (State d c) = (atCoord c (player d)) & pictureOfMaze

data State = State
  { _stateDirection :: Direction
  , _stateCoord :: Coord
  }

initialState :: State
initialState = State R (Coord (-3) 3)

resetableActivityOf ::
  forall world.
  world ->
  (Event -> world -> world) ->
  (world -> Picture) ->
  IO ()
resetableActivityOf w h draw = activityOf w eventHandler draw
  where
    eventHandler :: Event -> world -> world
    eventHandler e s =
      case e of
        (KeyPress "Esc") -> w
        _ -> h e s

exercise3 :: IO ()
exercise3 = resetableActivityOf initialState handleEvent drawState

maze :: Coord -> Tile
maze (Coord x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground
