{-# LANGUAGE LambdaCase #-}

module Dibujo (
    Dibujo,
    figura, rotar, espejar, rot45, apilar, juntar, modificar, encimar,
    r180, r270,
    (.-.), (///), (^^^),
    cuarteto, encimar4, ciclar,
    foldDib, mapDib,
    figuras
) where

import Internals.Dibujo

-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 = comp rotar 2

r270 :: Dibujo a -> Dibujo a
r270 = comp rotar 3

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = apilar 1.0 1.0

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = juntar 1.0 1.0

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = encimar

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = ( a /// b) .-. (c /// d)  

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a 
encimar4 a = (a ^^^ rotar a) ^^^ (r180 a ^^^ r270 a)

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar a =  cuarteto (rotar a) a (r180 a) (r270 a)

-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de Floatro a la lógica
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) -> (Float -> Float -> b -> b) ->
       Dibujo a -> b
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d =
    case d of
        Figura a -> fFig a
        Rotar d -> fRot (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d)
        Espejar d -> fEsp (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d)
        Rot45 d -> fRot45 (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d)
        Apilar x y d1 d2 -> fApi x y (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d1) 
                                     (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d2)
        Juntar x y d1 d2 -> fJun x y (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d1) 
                                     (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d2)
        Encimar d1 d2 -> fEnc (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d1) 
                              (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d2)
        Modificar x y d -> fMod x y (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d)

-- Demostrar que `mapDib figura = id`
mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib f (Figura a) = f a
mapDib f (Rotar d) = Rotar (mapDib f d) 
mapDib f (Espejar d) = Espejar (mapDib f d) 
mapDib f (Rot45 d) = Rot45 (mapDib f d) 
mapDib f (Apilar x y d1 d2) = Apilar x y (mapDib f d1) (mapDib f d2)
mapDib f (Juntar x y d1 d2) = Juntar x y (mapDib f d1) (mapDib f d2)
mapDib f (Encimar d1 d2) = Encimar (mapDib f d1) (mapDib f d2)
mapDib f (Modificar x y d) = Modificar x y (mapDib f d)

-- Junta todas las figuras básicas de un dibujo.
figuras :: Dibujo a -> [a]
figuras = foldDib (:[]) id id id (\_ _ a b -> a ++ b) (\_ _ a b -> a ++ b) (++) 
                  (\_ _ a -> a)
