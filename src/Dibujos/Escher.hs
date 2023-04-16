module Dibujos.Escher (
    interpBas,
    escherConf
) where

import Graphics.Gloss (blank, pictures, line, polygon)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar4, (^^^), (.-.), (///), espejar, r180, r270, cuarteto)
import FloatingPic (Output, half, zero, vacia)
import Interp (Conf(..), interp)

data Basica = Triangulo | Vacia
    deriving (Show, Eq)

type Escher = Basica

-- El dibujoU.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar4 (espejar (rot45 p))

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = p ^^^ (p1 ^^^ p2)
    where p1 = espejar (rot45 p)
          p2 = r270 p1

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 p = cuarteto fv fv fv (dibujoU p)
    where fv = figura Vacia
esquina n p = cuarteto e l (rotar l) (dibujoU p)
    where l = lado (n - 1) p
          e = esquina (n - 1) p

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 p = cuarteto fv fv (rotar (dibujoT p)) (dibujoT p)
    where fv = figura Vacia
lado n p = cuarteto l l (rotar (dibujoT p)) (dibujoT p)
    where l = lado (n - 1) p

noneto p q r s t u v w x = juntar 1 2 (api3 p s v /// api3 q t w) (api3 r u x)
    where api3 a b c = apilar 2 1 a (b .-. c)

-- El dibujo de Escher: squarelimit
escher :: Int -> Escher -> Dibujo Escher
escher n p = noneto (esquina n (figura p))
                    (lado n (figura p))
                    (r270 (esquina n (figura p)))
                    (rotar (lado n (figura p)))
                    (dibujoU (figura p))
                    (r270 (lado n (figura p)))
                    (rotar (esquina n (figura p)))
                    (r180 (lado n (figura p)))
                    (r180 (esquina n (figura p)))

interpBas :: Output Escher
interpBas Vacia x y w = vacia x y w
interpBas Triangulo x y w = pictures [line $ triangulo x y w, cara x y w]
  where
      triangulo x y w = map (x V.+) [zero, w, y, zero]
      cara x y w = polygon $ triangulo (x V.+ half w) (half y) (half w)

escherConf :: Conf
escherConf = Conf {
    name = "Escher",
    pic = interp interpBas (escher 5 Triangulo)
}