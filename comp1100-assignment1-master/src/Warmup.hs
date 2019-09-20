--
-- COMP1100/1130, Semester 1, 2018
--

module Main where
import CodeWorld


rectangle' :: Color -> Picture
rectangle' red' = colored red' (translated 0 (-2.5) (solidRectangle 1 2))

circle :: Color -> Picture
circle green' = colored green' (translated (-3) (6) (solidCircle 2))

ourPicture :: Picture
ourPicture = rectangle' red

main :: IO ()
main = drawingOf ourPicture


