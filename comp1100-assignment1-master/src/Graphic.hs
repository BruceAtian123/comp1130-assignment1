--
-- COMP1100/1130, Semester 1, 2018
--

module Graphic where
    
import Shape
import ColourName
import CodeWorld (Point)
type ZIndex = Int

data Graphic =
  Graphic Shape
          ColourName
          Point
          ZIndex

  deriving (Show)


between :: Double -> (Double, Double) -> Bool
b `between` (a, c) = a <= b && b <= c

between2 :: Double -> (Double, Double) -> Bool
b `between2` (a, c) = a <= b && b <= c || c <= b && b <= a

inGraphic :: Graphic -> Point -> Bool
inGraphic  (Graphic (Rectangle w h) _ (px, py) _) (x, y)
           = x `between` (px - w / 2, px + w / 2) && 
             y `between` (py - h / 2, py + h / 2)

inGraphic (Graphic (Ellipse w h) _ (px, py) _) (x, y)
           = let a = w / 2 
                 b = h / 2
                 x0 = x - px
                 y0 = y - py in 
                  (x0 / a) ** 2 + (y0 / b) ** 2 <= 1 + 1e-6

inGraphic (Graphic (Line (x1, y1) (x2, y2)) _ (px, py) _) (x,y)
          =  nearLine (x, y) ((px + x1, py + y1), (px + x2, py + y2))

inGraphic (Graphic (Polygon points) _ (px, py) _) (w,z) = inPolygon (w,z) ((map (\(_, _) -> (w + px, z + py))) points)

inGraphic (Graphic (Circle r) _ (px, py) _) (x,y) =
         r * r > (px - x) ** 2 + (py - y) ** 2 - 1e-6
(.-.) :: Point -> Point -> Point
a .-. b = (fst a - fst b, snd a - snd b)

(.+.) :: Point -> Point -> Point
a .+. b = (fst a + fst b, snd a + snd b)

(.*) :: Double -> Point -> Point
a .* b = (a * fst b, a * snd b)

dot :: Point -> Point -> Double
a `dot` b = fst a * fst b + snd a * snd b

dis :: Point -> Double
dis (x, y) = (x * x + y * y) ** 0.5
        
cross :: Point -> Point -> Double
a `cross` b = fst a * snd b - snd a * fst b


near :: Double -> Double -> Bool
a `near` b = abs( a - b) < 1e-6

nearPoint :: Point -> Point -> Bool
nearPoint (x1, y1) (x2, y2) = (x1 - x2)**2 + (y1 - y2)**2 < 1e-6


nearLine :: Point -> (Point, Point) -> Bool
nearLine p1 (p2, p3)
   =  abs((p2 .-. p1) `cross` (p3 .-. p1)) < 1e-6
   && fst p1 `between2` (fst p2, fst p3)
   && snd p2 `between2` (snd p2, snd p3)

crossLine :: (Point, Point) -> (Point, Point) -> Bool
crossLine (p1, p2) (p3, p4) = 
        let up    = (p3 .-. p1) `cross` (p4 .-. p1)
            down  = (p3 .-. p2) `cross` (p4 .-. p2) in
           if (up `near` 0) || (down `near` 0) || (up * down) `near` 0 then
                True
            else
                (up * down) < 0


inPolygon :: Point -> [Point] -> Bool
inPolygon pt pts = 
        let sides   = zip pts (tail pts ++ pts)
            farpoints = map (\x -> (10000, x + 10000)) [1..1000]
            intersects = [
                z  | a <- farpoints, 
                     let z = length [ '1' |
                                        y <- sides, 
                                        (pt, a) `crossLine` y,
                                        y `crossLine` (pt, a)]
                ]
           in
              any (nearPoint pt) pts ||
              any (nearLine pt)  sides ||
              length (filter odd intersects) > 800
              

            



              