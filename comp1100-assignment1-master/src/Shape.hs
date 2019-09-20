--
-- COMP1100/1130, Semester 1, 2018
--

module Shape where

import CodeWorld (Point)

data Shape = Rectangle Side Side
           | Ellipse   Side Side
           | Polygon   [Point]
           | Line      Point Point
           | Circle    Side
  deriving (Show)



data Tool = RectangleTool (Maybe Point)
          | EllipseTool   (Maybe Point)
          | LineTool      (Maybe Point)
          | PolygonTool   [Point]
          | CircleTool    (Maybe Point) (Maybe Point) (Maybe Point)
          | SelectTool    (Maybe Int)
          | RedrawTool Int Tool
          | WithPoint Tool (Maybe Point)
  deriving (Show)

toolKeyMap :: [(String, Tool)]
toolKeyMap = [
  ("R", RectangleTool Nothing),
  ("E", EllipseTool Nothing),
  ("P", PolygonTool []),
  ("L", LineTool Nothing),
  ("W", CircleTool Nothing Nothing Nothing),
  ("S", SelectTool Nothing)]
 




type Side = Double