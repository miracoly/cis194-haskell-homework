{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}

module Week1.Week1 (app) where

import CodeWorld

app :: IO ()
app = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))

midCircle :: Color -> Picture
midCircle c = colored c (translated 0 0 (solidCircle 1))

topCircle :: Color -> Picture
topCircle c = colored c (translated 0 2.5 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

data LightState = Green | Amber | Red | RedAmber

trafficLight :: LightState -> Picture
trafficLight Green = botCircle green & midCircle black & topCircle black & frame
trafficLight Amber = botCircle black & midCircle yellow & topCircle black & frame
trafficLight Red = botCircle black & midCircle black & topCircle red & frame
trafficLight RedAmber = botCircle black & midCircle yellow & topCircle red & frame

trafficController :: Double -> Picture
trafficController t
  | round (t / 8) <= 3 = trafficLight Green
  | round (t / 8) <= 4 = trafficLight Amber
  | round (t / 8) <= 7 = trafficLight Red
  | otherwise = trafficLight RedAmber

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

leaf :: Double -> Picture
leaf = solidCircle

type Bloom = Picture

tree :: Integer -> Bloom -> Picture
tree 0 b = b
tree n b =
  polyline [(0, 0), (0, 1)]
    & translated
      0
      1
      ( rotated (pi / 10) (tree (n -1) b) & rotated (- pi / 10) (tree (n -1) b)
      )

treeController :: Double -> Picture
treeController = tree 8 . leaf . (* 0.15) . sin

exercise2 :: IO ()
exercise2 = animationOf treeController

-- Exercise 3

wall :: Picture
wall = colored black (rectangle 1 1) & colored color $ solidRectangle 1 1
 where color = HSL 0.50 0.47 0.63

ground :: Picture
ground = colored color $ solidRectangle 1 1
 where color = HSL 0.50 0.21 0.87

storage :: Picture
storage = colored colDot (solidCircle 0.2) & ground
 where colDot = translucent red

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

data Tile = Wall | Ground | Storage | Box

drawTile :: Tile -> Picture
drawTile t =
  case t of
    Wall -> wall
    Ground -> ground
    Storage -> storage
    Box -> box

pictureOfMaze :: Picture
pictureOfMaze = foldr ((&) . f) blank coords
  where
    f :: (Integer, Integer) -> Picture
    f (x, y) = translated (fromIntegral x) (fromIntegral y) $ maybe blank drawTile $ maze x y
    coords :: [(Integer, Integer)]
    coords = [(x, y) | x <- [-10..10], y <- [-10..10]]

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze

maze :: Integer -> Integer -> Maybe Tile
maze x y
  | abs x > 4 || abs y > 4 = Nothing
  | abs x == 4 || abs y == 4 = Just Wall
  | x == 2 && y <= 0 = Just Wall
  | x == 3 && y <= 0 = Just Storage
  | x >= -2 && y == 0 = Just Box
  | otherwise = Just Ground