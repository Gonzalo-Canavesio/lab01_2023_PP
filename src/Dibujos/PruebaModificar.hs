module Dibujos.PruebaModificar(
    interpBas,
    pruebamodificarConf
) where
-- Prueba de modificar en base al dibujo ejemplo
import Graphics.Gloss (blank, pictures, line, polygon)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, modificar ,apilar )
import FloatingPic (Output, half, zero)
import Interp (Conf(..), interp)

type Basica = ()

ejemplo :: Dibujo Basica
ejemplo = figura ()

interpBas :: Output Basica
interpBas () a b c = pictures [line $ triangulo a b c, cara a b c]
  where
      triangulo a b c = map (a V.+) [zero, c, b, zero]
      cara a b c = polygon $ triangulo (a V.+ half c) (half b) (half c)

pruebamodificarConf :: Conf
pruebamodificarConf = Conf {
    name = "PruebaModificar",
    pic = interp interpBas (modificar 1.5 1.5 ejemplo)
}