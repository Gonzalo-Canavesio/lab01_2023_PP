module Dibujos.Grilla (grillaConf)
where

import Dibujo (Dibujo, figura)
import FloatingPic (Output, half, zero)
import Graphics.Gloss (Picture (Blank), text, translate, scale)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Grilla (grilla)
import Interp (Conf (..), interp)


type Basica = (Int, Int)

getGridItems :: Int -> [[Dibujo Basica]]
getGridItems n = [getRow x | x <- [0..n-1]]
  where
    getRow x = [ getItem x y | y <- [0..n-1] ]
    getItem x y = figura (x, y)

drawGrid :: Dibujo Basica
drawGrid = grilla $ getGridItems 8

interpBas :: Output Basica
interpBas gridItem (x, y) (sw, _) (_, sh) =  translate xPos yPos drawText
  where
    scaleFactor = 0.15
    xPos = x + (sw / 10) -- sw i squareWidth
    yPos = y + (sh / 3) -- sh i squareHeight
    drawText = scale scaleFactor scaleFactor $ text $ show gridItem


grillaConf :: Conf
grillaConf =
  Conf
    { name = "Grilla",
      pic = interp interpBas drawGrid
    }
