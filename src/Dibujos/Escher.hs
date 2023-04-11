module Dibujos.Escher (
    interpBas,
    escherConf
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

noneto p q r s t u v w x =  apilar 1.0 2.0
                            (juntar 1.0 2.0 p (juntar 1.0 1.0 q r))
                            (apilar 1.0 1.0 (juntar 1.0 2.0 s (juntar 1.0 1.0 t u)) 
                                        (juntar 1.0 2.0 v (juntar 1.0 1.0 w x)))

-- El dibujo de Escher: squarelimit
escher :: Int -> Escher -> Dibujo Escher
escher n p = noneto (esquina n (figura p)) --
                    (lado n (figura p)) -- 
                    (r270 (esquina n (figura p)))
                    (rotar (lado n (figura p))) --
                    (dibujoU (figura p)) --
                    (r270 (lado n (figura p)))
                    (rotar (esquina n (figura p)))
                    (r180 (lado n (figura p)))
                    (r180 (esquina n (figura p))) --

-- Rotar (Rotar (Rotar ()))

escher2 :: Int -> Escher -> Dibujo Escher
escher2 n p = (esquina n (figura p))

interpBas :: Output Escher
interpBas Vacia x y w = vacia x y w
interpBas Triangulo x y w = pictures [line $ triangulo x y w, cara x y w]
  where
      triangulo x y w = map (x V.+) [zero, w, y, zero]
      cara x y w = polygon $ triangulo (x V.+ half w) (half y) (half w)

escherConf :: Conf
escherConf = Conf {
    name = "Escher",
    pic = interp interpBas (escher2 2 Triangulo)
}