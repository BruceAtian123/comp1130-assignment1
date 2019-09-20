--
-- COMP1100/1130, Semester 1, 2018
--
module View where

import CodeWorld
import ColourName
import Graphic
import Shape
import State

import Data.Ord
import Data.List

import qualified Data.Text as Text
{-# ANN module ("HLint: ignore Redundant bracket"::String) #-}

drawState :: State -> Picture
drawState s@(World _ (WithPoint (RectangleTool (Just _)) (Just p2)) _) =
  let p = drawNewGraphic s (Just p2) in
    drawState p
drawState s@(World _ (WithPoint (EllipseTool (Just _)) (Just p2)) _) =
  let p = drawNewGraphic s (Just p2) in
    drawState p
drawState s@(World _ (WithPoint (LineTool (Just _)) (Just p2)) _) =
  let p = drawNewGraphic s (Just p2) in
    drawState p
drawState (World gs (PolygonTool pts@(_:_)) colour) =
  let p = drawNewGraphic (World gs (PolygonTool pts) colour) Nothing in
    drawState p
drawState (World gs (WithPoint (RedrawTool _ (RectangleTool (Just p1)) ) (Just p2)) colour) =
  let p = drawNewGraphic (World gs (RectangleTool (Just p1)) colour) (Just p2) in
    drawState p
drawState (World gs (WithPoint (RedrawTool _ (EllipseTool (Just p1)) ) (Just p2)) colour) =
  let p = drawNewGraphic (World gs (EllipseTool  (Just p1)) colour) (Just p2) in
    drawState p
drawState (World gs (WithPoint (RedrawTool _ (LineTool (Just p1)) ) (Just p2)) colour) =
  let p = drawNewGraphic (World gs (LineTool (Just p1)) colour) (Just p2) in
    drawState p
drawState (World gs (WithPoint (RedrawTool _ (PolygonTool pts@(_:_)) ) (Just p2)) colour) =
  let p = drawNewGraphic (World gs (PolygonTool pts) colour) (Just p2) in
    drawState p
drawState (World gs tool colour) =
  pictures $ shapeToPicture : colourToPicture : graphicsToPics gs

  where
    shapeToPicture, colourToPicture :: Picture
    shapeToPicture =
      translated (-13.5) 8 $ (text . Text.pack) ("Shape: " ++ shapeToText)
    colourToPicture =
      translated (-13.5) 7 $ (text . Text.pack) ("Colour: " ++ colourToText)
    shapeToText :: String
    shapeToText =
      let shape = takeWhile (/= ' ') $ show tool
       in take (length shape - 4) shape
    colourToText :: String
    colourToText = show colour

--g
-- | drawNewGraphic Test 1
--
-- >>> drawNewGraphic (World [] (RectangleTool (Just (-3, 3))) Black) (Just (3, -3))
-- World [Graphic (Rectangle 6.0 6.0) Black (0.0,0.0) 1] (RectangleTool Nothing) Black
--
-- | drawNewGraphic Test 2
--
-- >>> drawNewGraphic (World [] (RectangleTool (Just (0, 0))) Orange) (Just (3, 7))
-- World [Graphic (Rectangle 3.0 7.0) Orange (1.5,3.5) 1] (RectangleTool Nothing) Orange

drawNewGraphic :: State -> Maybe Point -> State
drawNewGraphic s (Just x) =
    --Debug.Trace.trace (show s) $
    case s of
    World gs (RectangleTool (Just _)) colour -> World (v:gs) (RectangleTool Nothing) colour
    World gs (EllipseTool (Just _)) colour -> World (v:gs) (EllipseTool Nothing) colour
    World gs (LineTool (Just _)) colour -> World (v:gs) (LineTool Nothing) colour
    World gs (PolygonTool _) colour -> World (v:gs) (PolygonTool []) colour
    World gs (WithPoint tool a) colour->
      let World gs' tool' _ = drawNewGraphic (World gs tool colour) (Just x) in
        World gs' (WithPoint tool' a) colour

    _ -> s
    where Just v = getNewGraphic s (Just x)

drawNewGraphic s@(World gs (PolygonTool _) colour) Nothing = World (v:gs) (PolygonTool []) colour
    where Just v = getNewGraphic s Nothing
drawNewGraphic _ Nothing = undefined


getNewGraphic' :: State -> Maybe Point -> Maybe Graphic
getNewGraphic' s z = case s of
        World _ (LineTool (Just (a,b))) colour -> case z of
                                        Just (c,d) -> Just (getLineGraphic (a,b) (c,d) colour)
                                        Nothing -> Nothing
        World _ (RectangleTool (Just (a,b))) colour -> case z of
                                             Just (c,d) -> Just (getRectangleGraphic (a,b) (c,d) colour)
                                             Nothing -> Nothing
        World _ (EllipseTool (Just (a,b))) colour -> case z of
                                                     Just (c,d) -> Just (getEllipseGraphic (a,b) (c,d) colour)
                                                     Nothing -> Nothing
        World _ (PolygonTool (ls)) colour -> case z of
                                                     Just (c,d) -> Just (getPolygonGraphic ((c,d):ls) colour)
                                                     Nothing -> Just (getPolygonGraphic ls colour)
        _ -> Nothing

getNewGraphic :: State -> Maybe Point -> Maybe Graphic
getNewGraphic s@(World gs _ _ ) z =
      case getNewGraphic' s z of
          (Just g) -> Just $ setZIndex (length gs + 1) g
          Nothing -> Nothing

getRectangleGraphic :: Point -> Point -> ColourName -> Graphic
getRectangleGraphic (a_x, a_y) (b_x, b_y) c =
                    Graphic (Rectangle x y)
                                c
                                z
                                0
 where (x,y,z) = getWidthHeightShift (a_x, a_y) (b_x, b_y)

getEllipseGraphic :: Point -> Point -> ColourName -> Graphic
getEllipseGraphic (a_x, a_y) (b_x, b_y) c =
                  Graphic (Ellipse x y)
                               c
                               z
                               0
 where (x,y,z) = getWidthHeightShift (a_x, a_y) (b_x, b_y)

getLineGraphic :: Point -> Point -> ColourName -> Graphic
getLineGraphic (a_x, a_y) (b_x, b_y) c =
               Graphic (Line (a_x,a_y) (b_x,b_y))
                          c
                          (0,0)
                          0

getPolygonGraphic :: [Point] -> ColourName -> Graphic
getPolygonGraphic x c =
               Graphic (Polygon (x))
                          c
                          (0,0)
                          0

getCircleGraphic :: Point -> Point -> Point -> ColourName -> Graphic
getCircleGraphic (a_x,a_y) (b_x,b_y) (c_x,c_y) colour =
              Graphic (Circle r)
                         colour
                         (m_x,m_y)
                         0
                         where
                            a = 2 * (b_x - a_x)
                            b = 2 * (b_y - a_y)
                            c = b_x * b_x + b_y * b_y - a_x * a_x - a_y * a_y
                            d = 2 * (c_x - b_x)
                            e = 2 * (c_y - b_y)
                            f = c_x * c_x + c_y * c_y - b_x * b_x - b_y * b_y
                            m_x = (b * f - e * c) / (b * d - e * a)
                            m_y = (d * c - a * f) / (b * d - e * a)
                            r   = sqrt (m_x - a_x) * (m_x - a_x) + (m_y - a_y) * (m_y - a_y)

getWidthHeightShift :: Point -> Point -> (Side, Side, Point)
getWidthHeightShift (a_x,a_y) (b_x,b_y) = ((abs (a_x-b_x)), (abs (a_y-b_y)), ((a_x+b_x)/2,(a_y+b_y)/2))

shapeToPic :: Shape -> Picture
shapeToPic x = case x of
   Rectangle x' y -> solidRectangle x' y
   Ellipse x' y  -> solidEllipse x' y
   Polygon x'  -> solidPolygon x'
   Circle x' -> solidCircle x'
   Line (x',y) (a,b)  -> polyline [(x',y), (a,b)]
   where solidEllipse x' y = scaled (x'/2) (y/2) (solidCircle 1)


zIndex :: Graphic -> Int
zIndex (Graphic _ _ _ z) = z

setZIndex :: Int -> Graphic -> Graphic
setZIndex z (Graphic a b c _) = Graphic a b c z

setElement :: Int -> a -> [a] -> [a]
setElement 0 a (_ : xs) = a : xs
setElement n a (x : xs) = x : setElement (n - 1) a xs
setElement _ _ []       = []

graphicsToPics :: [Graphic] -> [Picture]
graphicsToPics = map graphicToPic . sortBy (comparing (negate . zIndex))

graphicToPic :: Graphic -> Picture
graphicToPic a = case a of
    Graphic shape color (v,b) _ -> translated v b
        (colored (colourNameToColour color) (shapeToPic shape))