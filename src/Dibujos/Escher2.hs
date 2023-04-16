module Dibujos.Escher2 (
    interpBas,
    escher2Conf
) where

import Graphics.Gloss (blank, pictures, line, polygon)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, espejar, r180, r270, ciclar, cuarteto, (.-.), (^^^))
import FloatingPic (Output, half, zero, vacia)
import Interp (Conf(..), interp)

data Basica = Triangulo | Vacia
    deriving (Show, Eq)

type Escher = Basica

-- El dibujoU.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar (encimar p1 (rotar p1)) (encimar (rotar (rotar p1)) (rotar (rotar (rotar p1))) )
        where p1 = espejar (rot45 p)

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = encimar p (encimar p1 p2)
    where p1 = espejar (rot45 p)
          p2 = r270 p1

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 p = cuarteto (figura Vacia) (figura Vacia) (figura Vacia) (dibujoU p)
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) p) (rotar (lado (n-1) p)) (dibujoU p)

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 p = cuarteto (figura Vacia) (figura Vacia) (rotar (dibujoT p)) (dibujoT p)
lado n p = cuarteto (lado (n-1) p) (lado (n-1) p) (rotar (dibujoT p)) (dibujoT p)

noneto p q r s t u v w x y z a b c d e = apilar 1 1 (apilar 1 1 (juntar 1 1 (juntar 1 1 p q) (juntar 1 1 r s)) (juntar 1 1 (juntar 1 1 t u) (juntar 1 1 v w))) (apilar 1 1 (juntar 1 1 (juntar 1 1 x y) (juntar 1 1 z a)) (juntar 1 1 (juntar 1 1 b c) (juntar 1 1 d e)))

-- El dibujo de Escher: squarelimit
escher :: Int -> Escher -> Dibujo Escher
escher n p = noneto (esquina n (figura p))
                    (lado n (figura p))
                    (lado n (figura p))
                    (r270 (esquina n (figura p)))
                    (rotar (lado n (figura p)))
                    (dibujoU (figura p))
                    (dibujoU (figura p))
                    (r270 (lado n (figura p)))
                    (rotar (lado n (figura p)))
                    (dibujoU (figura p))
                    (dibujoU (figura p))
                    (r270 (lado n (figura p)))
                    (rotar (esquina n (figura p)))
                    (r180 (lado n (figura p)))
                    (r180 (lado n (figura p)))
                    (r180 (esquina n (figura p)))

interpBas :: Output Escher
interpBas Vacia x y w = vacia x y w
interpBas Triangulo x y w = pictures [line $ triangulo x y w, cara x y w]
  where
      triangulo x y w = map (x V.+) [zero, w, y, zero]
      cara x y w = polygon $ triangulo (x V.+ half w) (half y) (half w)

escher2Conf :: Conf
escher2Conf = Conf {
    name = "Escher2",
    pic = \t -> interp interpBas (escher (floor t `mod` 8 +1) Triangulo)
}