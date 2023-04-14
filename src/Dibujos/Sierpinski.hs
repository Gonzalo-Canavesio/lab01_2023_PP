module Dibujos.Sierpinski (
    interpBas,
    sierpinskiConf
) where

import Graphics.Gloss (blank, pictures, line, polygon, arc, translate, rotate)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, espejar, r180, r270, ciclar, cuarteto, (.-.), (^^^))
import FloatingPic (Output, half, zero, vacia)
import Interp (Conf(..), interp)

data Basica = Triangulo | Vacia
    deriving (Show, Eq)

type Sierpinski = Basica


sierp :: Int -> Sierpinski -> Dibujo Sierpinski
sierp 1 p = apilar 1 1 (juntar 1 3 (juntar 2 1 (figura Vacia) (figura p)) (figura Vacia)) (juntar 1 1 (figura p) (figura p))
sierp n p = apilar 1 1 (juntar 1 3 (juntar 2 1 (figura Vacia) (sierp (n-1) p)) (figura Vacia)) (juntar 1 1 (sierp (n-1) p) (sierp (n-1) p))

interpBas :: Output Sierpinski
interpBas Triangulo x y w = line $ map (x V.+) [(0,0), w V.+ half y, y, (0,0)]
interpBas Vacia x y w = vacia x y w

sierpinskiConf :: Conf
sierpinskiConf = Conf {
    name = "Sierpinski",
    pic = interp interpBas (sierp 8 Triangulo)
}