module Dibujos.Aureo (
    interpBas,
    aureoConf
) where

import Graphics.Gloss (blank, pictures, line, polygon, arc, translate, rotate)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, espejar, r180, r270, ciclar, cuarteto, (.-.), (^^^))
import FloatingPic (Output, half, zero, vacia)
import Interp (Conf(..), interp)

data Basica = RectanguloConDiagonal | Vacia
    deriving (Show, Eq)

type Aurea = Basica

proporcion :: Int -> Dibujo Aurea -> Dibujo Aurea
proporcion 1 p = figura RectanguloConDiagonal
proporcion n p = juntar  1 1.6180339887 (figura RectanguloConDiagonal) (r270 (proporcion (n-1) p))

interpBas :: Output Aurea
interpBas RectanguloConDiagonal x y w = pictures [line rectangulo, line (cuartoCircunferencia x y w)]
    where 
        rectangulo = [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]
        cuartoCircunferencia x y w = map (\i -> ((fst i) V.* y) V.+ (snd i V.* w) V.+ x) (puntos)
        puntos = map ((1,0) V.+) [((-1) * sin(i*pi/100), cos(i*pi/100)) | i <- [0..49]]


interpBas Vacia x y w = vacia x y w

aureoConf :: Conf
aureoConf = Conf {
    name = "Aureo",
    pic = interp interpBas (apilar 1.6180339887 1 (figura Vacia) (proporcion 20 (figura RectanguloConDiagonal)))
}