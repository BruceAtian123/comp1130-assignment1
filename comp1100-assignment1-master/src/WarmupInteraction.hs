{-# LANGUAGE OverloadedStrings #-}
module Main where
import CodeWorld

type Coord = (Double, Double)

initialCoord :: Coord
initialCoord = (0,0)

main :: IO()
main = interactionOf initialCoord handleTime handleEvent drawState

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent e (x,y) = case e of
  KeyPress "Up" -> (x,y+1)
  KeyPress "Down" -> (x,y-1)
  KeyPress "Left" -> (x-1,y)
  KeyPress "Right" -> (x+1,y)
  _ -> (x,y)

drawState :: Coord -> Picture
drawState (x,y) = translated x y (solidCircle 1)

