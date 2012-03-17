-- hw1.hs
-- Pierre Fourgeaud
-- in group with Pierre-Louis Gottfrois
--

import SOE

data Shape =  Rectangle Float Float
            | Ellipse Float Float
            | RtTriangle Float Float
            | Polygon [(Float, Float)]
            deriving Show

--
circle, square :: Float -> Shape
circle radius  = Ellipse radius radius
square side     = Rectangle side side

--
area :: Shape -> Float
area (Rectangle s1 s2)  = s1 * s2
area (Ellipse r1 r2)    = pi * r1 * r2
area (RtTriangle s1 s2) = (s1 * s2) / 2
area (Polygon (v1:pts)) = polyArea pts
    where polyArea :: [(Float, Float)] -> Float
          polyArea (v2:v3:vs) = triArea v1 v2 v3 + polyArea (v3:vs)
          polyArea _          = 0

triArea v1 v2 v3 =
    let a = distBetween v1 v2
        b = distBetween v2 v3
        c = distBetween v3 v1
        s = 0.5 * (a + b + c)
    in sqrt (s * (s - a) * (s - b) * (s - c))

distBetween (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- FUNCTIONS
rectangle vertices   = Polygon vertices
rtTriangle vertices  = Polygon vertices

-- SIDES
sides :: Shape -> Integer
sides (Rectangle s1 s2)   = 4
sides (Ellipse r1 r2)     = 42
sides (RtTriangle s1 s2)  = 3
sides (Polygon pts) =
        if length(pts) == 0 || area (Polygon pts) == 0 then 0
        else toInteger(length pts)

-- BIGGER
-- We choose to extend the perimeter
-- `e` represent the factor
bigger :: Shape -> Float -> Shape
bigger (Rectangle s1 s2) e  = Rectangle (s1 * e) (s2 * e)
bigger (Ellipse r1 r2) e    = Ellipse (r1 * e) (r2 * e)
bigger (RtTriangle s1 s2) e = RtTriangle (s1 * e) (s2 * e)
bigger (Polygon pts) e      = Polygon [(x * e, y * e) | (x, y) <- pts]

-- HANOI
hanoi :: Int -> String -> String -> String -> IO ()
hanoi 1 a b c         = putStr("move disc from " ++ a ++ " to " ++ b ++ "\n")
hanoi n a b c         = 
          do hanoi (n - 1) a c b
             putStr("move disc from " ++ a ++ " to " ++ b ++ "\n")
             hanoi (n - 1) c b a

-- FRACTAL
fillSquare x y size w = drawInWindow w (withColor SOE.Blue (polygon [(x, y), (x + size, y), (x + size, y - size), (x, y - size)]))

minSize = 1

sierpinskiSquare w x y size =
      if size <= minSize
        then fillSquare x y size w
        else  let size2 = size `div` 3
              in do sierpinskiSquare w x y size2
                    sierpinskiSquare w x (y - size2) size2
                    sierpinskiSquare w x (y - (size2 * 2)) size2
                    sierpinskiSquare w (x + size2) (y - (size2 * 2)) size2
                    sierpinskiSquare w (x + (size2 * 2)) (y - (size2 * 2)) size2
                    sierpinskiSquare w (x + (size2 * 2)) (y - size2) size2
                    sierpinskiSquare w (x + (size2 * 2)) (y) size2
                    sierpinskiSquare w (x + size2) (y) size2

sierpinskiCarpet = runGraphics $ do
  w <- openWindow "Sierpinski Carpet" (385, 355)
  sierpinskiSquare w 50 320 291
  k <- getKey w
  closeWindow w


-- MY FRACTAL
-- We used a model called fern
drawLine px py cx cy w = drawInWindow w (withColor SOE.Green (polyline [(round px, round py), (round cx, round cy)]))

recursion       = 10
bendAngle       = 12 * (pi / 180)
branchAngle     = 70 * (pi / 180)
trunkRatio      = 0.15
antiTrunkRatio  = 1 - trunkRatio
branchRatio     = 0.5
heightScale     = 1.5

fern w px py a rad level = do
        let cx = px - (cos(a) * rad * trunkRatio) in do
        let cy = py - (sin(a) * rad * trunkRatio) in do
        drawLine px py cx cy w
        if level > 0
        then do
          fern w cx cy (a + bendAngle - branchAngle) (rad * branchRatio) (level - 1)
          fern w cx cy (a + bendAngle + branchAngle) (rad * branchRatio) (level - 1)
          fern w cx cy (a + bendAngle) (rad * antiTrunkRatio) (level - 1)
        else putStr("Wait ...\r")

myFractal = runGraphics $ do
  w <- openWindow "Beautiful Fractal" (600, 600)
  fern w 240 570 (pi / 2) (500 * heightScale) recursion
  k <- getKey w
  closeWindow w
