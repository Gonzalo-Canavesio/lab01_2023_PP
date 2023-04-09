module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP
) where

import Dibujo(Dibujo, foldDib, mapDib)

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar Pred p f dib = mapDib(\x -> if p x then f x else x) dib
-- Para esta funcion utilizamos la funcion mapdib donde utilizamos una funcion lambda
-- que chequea si el predicado se cumple para la figura basica, si se cumple se aplica la funcion f en caso contrario se devuelve la figura basica

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p dib = foldDib p id id id (\a b x y -> x || y) (\a b x y -> x || y) (\x y -> x || y) dib 

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p dib = foldDib p id id id (\a b x y -> x && y) (\a b x y -> x && y) (\x y -> x && y) dib

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p q = \x -> p x && q x

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p q = \x -> p x || q x
