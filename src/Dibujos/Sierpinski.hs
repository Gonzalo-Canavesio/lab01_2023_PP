module Dibujos.Sierpinski (
    interpBas,
    sierpinskiConf
) where

import Graphics.Gloss (line)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, (.-.), (///))
import FloatingPic (Output, half, zero, vacia)
import Interp (Conf(..), interp)

data Basica = Triangulo | Vacia
    deriving (Show, Eq)

type Sierpinski = Basica


sierp :: Int -> Sierpinski -> Dibujo Sierpinski
sierp 1 p = juntar 1 3 (juntar 2 1 fv fp) fv .-. (fp /// fp)
    where fp = figura p
          fv = figura Vacia
sierp n p = juntar 1 3 (juntar 2 1 fv s) fv .-. (s /// s)
    where s = sierp (n-1) p
          fv = figura Vacia

interpBas :: Output Sierpinski
interpBas Triangulo x y w = line $ map (x V.+) [(0,0), w V.+ half y, y, (0,0)]
interpBas Vacia x y w = vacia x y w

sierpinskiConf :: Conf
sierpinskiConf = Conf {
    name = "Sierpinski",
    pic = interp interpBas (sierp 8 Triangulo)
}