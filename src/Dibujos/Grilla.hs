module Dibujos.Grilla (grillaConf)
where

import Dibujo (Dibujo, figura, rotar, rot45, r180, r270, ciclar, cuarteto, (.-.), (///), apilar, juntar, encimar, espejar)
import FloatingPic (Output, half, zero)
import Graphics.Gloss (Picture (Blank), blue, color, line, pictures, red, white, polygon, text, translate, scale)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Grilla (grilla)
import Interp (Conf (..), interp)


type Basica = (Int, Int)

getGridItems n = [getRow x | x <- [0..n-1]]
  where
    getRow x = [ getItem x y | y <- [0..n-1] ]
    getItem x y = figura (x, y)

drawGrid = grilla $ getGridItems 8


interpBas :: Output Basica
interpBas gridItem (x, y) (squareWidth, _) (_, squareHeight) =  translate xPos yPos $ scale scaleFactor scaleFactor $ text $ show gridItem
  where
    scaleFactor = 0.15
    xPos = x + (squareWidth / 10)
    yPos = y + (squareHeight / 3)

grillaConf :: Conf
grillaConf =
  Conf
    { name = "Grilla",
      pic = \t -> interp interpBas drawGrid
    }
