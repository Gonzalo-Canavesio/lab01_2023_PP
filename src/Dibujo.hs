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

-- Nos basamos en la siguiente respuesta de stack overflow: https://stackoverflow.com/a/14379426
-- Y decidimos poner los miembros privados del modulo en Internals.Dibujo
-- para poder testear comodamente sin perder la encapsulacion
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
-- Junto a las figuras a y b en la parte de arriba donde a está a la izquierda y b a la derecha.
-- Junto a las figuras c y d en la parte de abajo donde c está a la izquierda y d a la derecha.

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a 
encimar4 a = r270 a ^^^ (r180 a ^^^ ( rotar a ^^^ a))

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
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod (Figura d) = fFig d -- si d es una figura, aplico fFig a d
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod (Rotar d) = fRot (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d)  -- Si d es una rotacion, aplico foldDib a d para obtener el dibujo y aplico fRot a ese dibujo
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod (Espejar d) = fEsp (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d) -- lo mismo que rotar
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod (Rot45 d) = fRot45 (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d) -- lo mismo que rotar
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod (Apilar x y d1 d2) = fApi x y (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d1) (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d2) -- si d es una apilacion, aplico foldDib a d1 y d2 para obtener los dibujos y aplico fApi a esos dibujos
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod (Juntar x y d1 d2) = fJun x y (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d1) (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d2) -- lo mismo que apilar
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod (Encimar d1 d2) = fEnc (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d1) (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d2) -- si d es una encimacion, aplico foldDib a d1 y d2 para obtener los dibujos y aplico fEnc a esos dibujos
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod (Modificar x y d) = fMod x y (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc fMod d) -- si d es una modificacion, aplico foldDib a d para obtener el dibujo y aplico fmod a ese dibujo


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
figuras = foldDib (:[]) id id id (\_ _ a b -> a ++ b) (\_ _ a b -> a ++ b) (++) (\_ _ a -> a)
-- (:[])  haciendo referencia a fFig de foldDib, que cada vez que encuentra una figura, la agrega a una lista con solo esa figura
-- id haciendo referencia a fRot, fEsp y fRot45 de foldDib, que cada vez que encuentra una rotacion, espejado o rotacion de 45, no hace nada.
-- (\_ _ a b -> a ++ b) haciendo referencia a fApi y fJun de foldDib, que cada vez que encuentra una apilacion o una juntacion, concatena las listas de figuras de los dos dibujos
-- (++) haciendo referencia a fEnc de foldDib, que cada vez que encuentra una encimacion, concatena las listas de figuras de los dos dibujos

