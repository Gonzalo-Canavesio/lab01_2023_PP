module Dibujos.Aureo (
    interpBas,
    aureoConf
) where

import Graphics.Gloss (pictures, line)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, apilar, r270)
import FloatingPic (Output, vacia)
import Interp (Conf(..), interp)

data Basica = RectanguloConCircunf | Vacia
    deriving (Show, Eq)

type Aurea = Basica

razonAurea :: Float
razonAurea = 1.6180339887

proporcion :: Int -> Dibujo Aurea -> Dibujo Aurea
proporcion 1 p = figura RectanguloConCircunf
proporcion n p = juntar  1 razonAurea (figura RectanguloConCircunf) 
                                      (r270 (proporcion (n - 1) p))

interpBas :: Output Aurea
interpBas Vacia x y w = vacia x y w
interpBas RectanguloConCircunf x y w = pictures [line rectangulo, 
                                                 line (cuartoCircunferencia x y w)]
    where 
        rectangulo = [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]
        cuartoCircunferencia x y w = map (\i -> i1 i V.+ i2 i V.+ x) puntos
        i1 i = fst i V.* y
        i2 i = snd i V.* w
        puntos = [(1,0) V.+ ((-1) * sin(i*pi/100), cos(i*pi/100)) | i <- [0..49]]

aureoConf :: Conf
aureoConf = Conf {
    name = "Aureo",
    pic = interp interpBas (apilar razonAurea 1 (figura Vacia) 
                                   (proporcion 20 (figura RectanguloConCircunf)))
}
