module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP
) where

import Dibujo(Dibujo, foldDib, mapDib, figura)

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f = mapDib(\x -> if p x then f x else figura x)

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib p id id id (\_ _ x y -> x || y) (\_ _ x y -> x || y) (||) 
                  (\_ _ x -> x)

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p = foldDib p id id id (\_ _ x y -> x && y) (\_ _ x y -> x && y) (&&)
                   (\_ _ x -> x)

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p q x = p x && q x

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p q x = p x || q x
