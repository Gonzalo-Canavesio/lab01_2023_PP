module Dibujos.PruebaModificar(
    interpBas,
    pruebaModificarConf
) where
-- Prueba de modificar en base al dibujo ejemplo
import Graphics.Gloss (blank, pictures, line, polygon)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, modificar, (^^^))
import FloatingPic (Output, half)
import Interp (Conf(..), interp)

data Basica = Hexagono
    deriving (Show, Eq)

interpBas :: Output Basica
interpBas Hexagono x y w = line . map (x V.+) $ puntos
    where puntos = [base V.+ pIzq, base V.+ pDer, y V.+ half w, techo V.+ pDer, 
                    techo V.+ pIzq, half w, base V.+ pIzq]
          base = 0.1 V.* w
          techo = 0.9 V.* w
          pIzq = 0.27 V.* y
          pDer = 0.73 V.* y

usoModificar :: Int -> Basica ->Dibujo Basica
usoModificar 1 p = figura p
usoModificar n p = figura p ^^^ modificar 0.9 0.9 (usoModificar (n - 1) p)

pruebaModificarConf :: Conf
pruebaModificarConf = Conf {
    name = "PruebaModificar",
    pic = interp interpBas (modificar 2 2 (usoModificar 500 Hexagono))
}