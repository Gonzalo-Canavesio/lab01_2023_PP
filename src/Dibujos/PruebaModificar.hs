module Dibujos.PruebaModificar(
    interpBas,
    pruebaModificarConf
) where
-- Prueba de modificar en base al dibujo ejemplo
import Graphics.Gloss (blank, pictures, line, polygon)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, modificar ,apilar, encimar )
import FloatingPic (Output, half, zero)
import Interp (Conf(..), interp)

type Basica = ()

ejemplo :: Dibujo Basica
ejemplo = figura ()

interpBas :: Output Basica
interpBas () x y w = line . map (x V.+) $ [0.1 V.* w V.+ 0.27 V.* y, 0.1 V.* w V.+ 0.73 V.* y, y V.+ half w, 0.9 V.* w V.+ 0.73 V.* y, 0.9 V.* w V.+ 0.27 V.* y, half w, 0.1 V.* w V.+ 0.27 V.* y]

usoModificar :: Int -> Dibujo Basica
usoModificar 1 = figura ()
usoModificar n = encimar (figura ()) (modificar 0.9 0.9 (usoModificar (n-1)))

pruebaModificarConf :: Conf
pruebaModificarConf = Conf {
    name = "PruebaModificar",
    pic = interp interpBas (modificar 2 2 (usoModificar 500))
}