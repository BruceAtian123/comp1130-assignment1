--
-- COMP1100/1130, Semester 1, 2018
--
module Main where

import CodeWorld (interactionOf)
import Events (handleEvent)
import View (drawState)
import State (State, initialState)

main :: IO ()
main = interactionOf initialState handleTime handleEvent drawState



handleTime :: Double -> State -> State
handleTime _ s = s
