--
-- COMP1100/1130, Semester 1, 2018
--

{-# LANGUAGE OverloadedStrings #-}

module Events where

import CodeWorld hiding (trace)
import Debug.Trace
import State
import ColourName
import Shape
import Graphic
import View

--
-- | initialState Test
--
-- >>> initialState
-- World [] (RectangleTool Nothing) Black

--
-- | Rectangle Event Test
--
-- >>> :set -XOverloadedStrings
-- >>> handleEvent (KeyPress "R") initialState
-- World [] (RectangleTool Nothing) Black

--
-- | Incorrect Shape Input Test
--
-- >>> :set -XOverloadedStrings
-- >>> handleEvent (KeyPress "X") initialState
-- World [] (RectangleTool Nothing) Black

--
-- | Magenta Colour Event Test
--
-- >>> :set -XOverloadedStrings
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (RectangleTool Nothing) Black
-- >>> handleEvent (KeyPress "M") state
-- World [] (RectangleTool Nothing) Magenta

--
-- | Green Colour Event Test
--
-- >>> :set -XOverloadedStrings
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (RectangleTool Nothing) Black
-- >>> handleEvent (KeyPress "G") state
-- World [] (RectangleTool Nothing) Green

--
-- | Incorrect Colour Input Test
--
-- >>> :set -XOverloadedStrings
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (RectangleTool Nothing) Black
-- >>> handleEvent (KeyPress "H") state
-- World [] (RectangleTool Nothing) Black

--
-- | MousePress Event Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (RectangleTool Nothing) Cyan
-- >>> handleEvent (MousePress LeftButton (-3, 3)) state
-- World [] (WithPoint (RectangleTool (Just (-3.0,3.0))) Nothing) Cyan

--
-- | MouseRelease Event Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (RectangleTool (Just (-3.0,3.0))) Cyan
-- >>> handleEvent (MouseRelease LeftButton (3, -3)) state
-- World [Graphic (Rectangle 6.0 6.0) Cyan (0.0,0.0) 1] (RectangleTool Nothing) Cyan

--
-- | Polygon Event Test
--
-- >>> :set -XOverloadedStrings
-- >>> handleEvent (KeyPress "P") initialState
-- World [] (PolygonTool []) Black

--
-- | Polygon Colour Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> :set -XOverloadedStrings
-- >>> let state = World [] (PolygonTool []) Black
-- >>> handleEvent (KeyPress "O") state
-- World [] (PolygonTool []) Orange

--
-- | Polygon Vertex 1 Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (PolygonTool []) Orange
-- >>> handleEvent (MousePress LeftButton (3, 5)) state
-- World [] (WithPoint (PolygonTool [(3.0,5.0)]) Nothing) Orange

--
-- | Polygon Vertex 2 Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (PolygonTool [(3.0,5.0)]) Green
-- >>> handleEvent (MousePress LeftButton (6, 8)) state
-- World [] (WithPoint (PolygonTool [(6.0,8.0),(3.0,5.0)]) Nothing) Green

--
-- | Polygon Vertex 3 Test
--
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (PolygonTool [(6.0,8.0),(3.0,5.0)]) Green
-- >>> handleEvent (MousePress LeftButton (9, 2)) state
-- World [] (WithPoint (PolygonTool [(9.0,2.0),(6.0,8.0),(3.0,5.0)]) Nothing) Green

--
-- | Polygon Draw Test
--
-- >>> :set -XOverloadedStrings
-- >>> import Shape
-- >>> import ColourName
-- >>> let state = World [] (PolygonTool [(9.0,2.0),(6.0,8.0),(3.0,5.0)]) Yellow
-- >>> handleEvent (KeyPress " ") state
-- World [Graphic (Polygon [(9.0,2.0),(6.0,8.0),(3.0,5.0)]) Yellow (0.0,0.0) 1] (PolygonTool []) Yellow

--
-- | Shape Removal Test
--
-- >>> :set -XOverloadedStrings
-- >>> import Shape
-- >>> import ColourName
-- >>> import Graphic
-- >>> let state = World [(Graphic (Rectangle 6 6) Cyan (0, 0) 0), (Graphic (Rectangle 3 3) Magenta (8, 4) 0)] (RectangleTool Nothing) Orange
-- >>> handleEvent (KeyPress "Backspace") state
-- World [Graphic (Rectangle 3.0 3.0) Magenta (8.0,4.0) 0] (RectangleTool Nothing) Orange

adjust :: State -> State
adjust (World (x:xs) a b)
  = World (y:xs) a b
    where y = setZIndex (length xs + 1) x

adjust s = s


mapTool :: Graphic -> Tool
mapTool (Graphic (Rectangle _ _) _ _ _) = RectangleTool Nothing
mapTool (Graphic (Ellipse _ _) _ _ _) = EllipseTool Nothing
mapTool (Graphic (Circle _) _ _ _) = CircleTool Nothing Nothing Nothing
mapTool (Graphic (Line _ _) _ _ _) = LineTool Nothing
mapTool (Graphic (Polygon _) _ _ _) = PolygonTool []

isPolygonTool :: Tool -> Bool
isPolygonTool (PolygonTool _) = True
isPolygonTool _ = False



getPolygonPoints :: Tool -> [Point]
getPolygonPoints (PolygonTool pts) = pts
getPolygonPoints _ = []

findSelectedGraphic :: State -> Point -> Maybe Int
findSelectedGraphic (World gs _ _) pt =
      let g  = zip gs [0..]
          g' = filter (\(w,_) -> inGraphic w pt) g
         in
          if null g' then
            Nothing
          else
            let z  = map (\(w, i) -> (zIndex w, -i)) g'
                m  = maximum z
               in
                Just (- (snd m))

withpoint :: State -> State
withpoint (World ls t c) = World ls (WithPoint t Nothing) c

setpoint :: State -> Point -> State
setpoint (World ls (WithPoint t _) c) pt = World ls (WithPoint t (Just pt)) c
setpoint a _ = a

getpoint :: State -> Maybe Point
getpoint (World _ (WithPoint _ p) _)  = p
getpoint _ = Nothing

withoutpoint :: State -> State
withoutpoint (World ls (WithPoint t _) c) = World ls t c
withoutpoint a = a




handleEvent :: Event -> State -> State
handleEvent (KeyPress "L") (World ls (SelectTool (Just index)) colour) =
  let g  = ls !! index
      g' = setZIndex (zIndex g - 1) g
      ls' = setElement index g' ls
     in
      World ls' (SelectTool (Just index)) colour

handleEvent (KeyPress "P") (World ls (SelectTool (Just index)) colour) =
  let g  = ls !! index
      g' = setZIndex (zIndex g + 1) g
      ls' = setElement index g' ls
     in
      World ls' (SelectTool (Just index)) colour

handleEvent (KeyPress "Esc") (World ls (SelectTool (Just _)) colour) =
    World ls (SelectTool Nothing) colour

handleEvent (KeyPress " ") (World ls (SelectTool (Just index)) colour) =
    World ls (RedrawTool index (mapTool (ls !! index))) colour

handleEvent e s@(World ls tool colour) =
  --Debug.Trace.trace (show tool) $
  case e of
    KeyPress key
      | key == "Esc" -> initialState
      | key == "D"   -> trace (show s) s
      | key == "E" -> World ls (EllipseTool Nothing) colour
      | key == "R" -> World ls (RectangleTool Nothing) colour
      | key == "P" -> World ls (PolygonTool []) colour
      | key == "L" -> World ls (LineTool Nothing) colour
      | key == "W" -> World ls (CircleTool Nothing Nothing Nothing) colour
      | key == "S" -> World ls (SelectTool Nothing) colour
      | key == "B" -> World ls tool Black
      | key == "M" -> World ls tool Magenta
      | key == "G" -> World ls tool Green
      | key == "Y" -> World ls tool Yellow
      | key == "O" -> World ls tool Orange
      | key == "C" -> World ls tool Cyan
      | key == " " -> case s of
                 World [] (PolygonTool  (x:xs)) _  -> adjust $ World [getPolygonGraphic (x:xs) colour] (PolygonTool []) colour
                 World fs (PolygonTool (x:xs)) _  -> adjust $ World (getPolygonGraphic (x:xs) colour:fs) (PolygonTool []) colour
                 _->s
      | key == "Backspace" -> case s of
                 World [_] _ _ -> World [] tool colour
                 World (_:fs) _ _ -> World fs tool colour
                 _->s


    MousePress LeftButton (x,y) -> withpoint $ case s of
       World gs (RectangleTool Nothing) _ ->  World gs (RectangleTool (Just (x,y))) colour
       World gs (LineTool Nothing) _-> World gs (LineTool (Just (x,y))) colour
       World gs (EllipseTool Nothing) _ -> World gs (EllipseTool (Just (x,y))) colour
       World gs (PolygonTool []) _ -> adjust $ World gs (PolygonTool  [(x,y)]) colour
       World gs (PolygonTool hs) _ ->  adjust $ World gs (PolygonTool ((x,y):hs)) colour
       World gs (RedrawTool index a) c ->
          let (World _ tool' c') = withoutpoint $ handleEvent e (World [] a c) in
              World gs (RedrawTool index tool') c'
       _ -> s
    MouseMovement (x, y) -> setpoint s (x,y)
    MouseRelease LeftButton (z,v) -> withoutpoint $ case s of
      World gs (RectangleTool (Just (x,y))) _ -> adjust $ World (getRectangleGraphic (x,y) (z,v) colour:gs) (RectangleTool Nothing) colour
      World gs (LineTool (Just (x,y))) _ -> adjust $  World (getLineGraphic (x,y) (z,v) colour:gs) (LineTool Nothing) colour
      World gs (EllipseTool (Just (x,y))) _ -> adjust $  World (getEllipseGraphic (x,y) (z,v) colour:gs) (EllipseTool Nothing) colour
      World gs (PolygonTool  [(x,y)]) _-> adjust $ World gs (PolygonTool  [(x,y)]) colour
      World gs (PolygonTool ((x,y):hs)) _-> adjust $ World gs (PolygonTool ((x,y):hs)) colour
      World gs (CircleTool Nothing Nothing Nothing) _-> World gs (CircleTool (Just(z,v)) Nothing Nothing) colour
      World gs (CircleTool (Just(x,y)) Nothing Nothing) _-> World gs (CircleTool (Just (x,y)) (Just (z,v)) Nothing) colour
      World gs (CircleTool (Just(x,y)) (Just(a,b)) Nothing) _ -> adjust $ World (getCircleGraphic (x,y) (a,b) (z,v) colour:gs) (CircleTool Nothing Nothing Nothing) colour
      World gs (SelectTool _) _ ->
        World gs (SelectTool (findSelectedGraphic s (z, v))) colour
      World gs (RedrawTool index r) c->
        let (World gs' tool' c') = handleEvent e (World [] r c)  in
          if not (null gs') then
            let newly = head gs'
                gs'' = setElement index newly gs
               in
                World gs'' (RedrawTool index tool') c'
          else if isPolygonTool tool' then
            let
                polygon' = gs !! index
                (Graphic (Polygon points) _ _ _) = polygon'
                z' = zIndex (gs !! index)
                newgraphic = setZIndex z' (getPolygonGraphic (points ++ getPolygonPoints tool') c')
                gs'' = setElement index newgraphic gs
               in
                World gs'' (RedrawTool index (PolygonTool [])) c'
          else
            World gs (RedrawTool index tool') c'
      World gs (WithPoint d _) _ ->
         handleEvent e (World gs d colour)


      _->s


    _ -> s