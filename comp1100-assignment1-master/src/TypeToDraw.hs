--
-- COMP1100/1130, Semester 1, 2018
--

module Main where

import View
import Shape
import ColourName
import CodeWorld
import Graphic

import Safe (readMay)
import Data.Maybe (maybe)

main :: IO ()
main = do
    graphics <- addGraphic [] >>= newGraphicPrompt
    drawingOf $ pictures $ graphicsToPics graphics
    
newGraphicPrompt :: [Graphic] -> IO [Graphic]
newGraphicPrompt gs = do
  putStrLn "Was that the last shape you wanted to draw? (Y/N)"
  lastG <- getLine
  case lastG of
    "Y" -> return gs
    "N" -> addGraphic gs >>= newGraphicPrompt
    _   -> putStrLn "Y/N only." >> newGraphicPrompt gs

addGraphic :: [Graphic] -> IO [Graphic]
addGraphic gs = do
  t <- selectTool
  c <- selectColour
  case t of
    PolygonTool _ -> do
      points <- getPoints
      return $ getPolygonGraphic points c : gs
    _             -> do
      start <- selectPoint "starting"
      end   <- selectPoint "ending"
      return $ case t of
        LineTool _      -> getLineGraphic start end c : gs
        RectangleTool _ -> getRectangleGraphic start end c : gs
        EllipseTool _   -> getEllipseGraphic start end c : gs
        _               -> gs
        
selectTool :: IO Tool
selectTool = do
  putStrLn "Which shape are you trying to draw?"
  putStrLn "(R)ectangle\n(L)ine\n(P)olygon\n(E)llipse"
  shape <- getLine
  maybe selectTool pure $ lookup shape toolKeyMap
        
selectColour :: IO ColourName
selectColour = do
  putStrLn "Which colour are you trying to use?"
  putStrLn "(M)agenta\n(B)lack\n(G)reen\n(Y)ellow\n(O)range\n(C)yan"
  colour <- getLine
  maybe selectColour pure $ lookup colour colourKeyMap
        
selectPoint :: String -> IO (Point)
selectPoint s = do
  putStrLn $ "Enter " ++ s ++ " coordinates as a tuple (x, y):"
  readMay <$> getLine >>=
    maybe (putStrLn "Input error." >> selectPoint s) pure
    
getPoints :: IO [Point]
getPoints = getPoints' [] 
  where
    getPoints' :: [Point] -> IO [Point]
    getPoints' l
      | length l >= 3 = do
        putStrLn "Is this the last vertex? (Y/N)"
        lastV <- getLine
        case lastV of
          "Y" -> return l
          "N" -> getMoreVertices l
          _   -> putStrLn "Y/N only." >> getPoints' l
      | otherwise = getMoreVertices l
            
    getMoreVertices :: [Point] -> IO [Point]
    getMoreVertices l = do
      putStrLn "Enter additional vertex as a tuple (x, y):"
      readMay <$> getLine >>= maybe
        (putStrLn "Input error. Try again" >> getPoints' l)
        (\x -> getPoints' (x : l))