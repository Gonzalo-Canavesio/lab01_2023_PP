{-# LANGUAGE LambdaCase #-}
module Dibujo (
    Dibujo,
    figura, rotar, espejar, rot45, apilar, juntar, encimar,
    r180, r270,
    (.-.), (///), (^^^),
    cuarteto, encimar4, ciclar,
    foldDib, mapDib,
    figuras
) where


{-
--Gramática de las figuras:
<Fig> ::= Figura <Bas> | Rotar <Fig> | Espejar <Fig> | Rot45 <Fig>
    | Apilar <Float> <Float> <Fig> <Fig> 
    | Juntar <Float> <Float> <Fig> <Fig> 
    | Encimar <Fig> <Fig>
-}

data Dibujo a = Figura a | Rotar (Dibujo a) | Espejar (Dibujo a) 
    | Rot45 (Dibujo a)
    | Apilar Float Float (Dibujo a) (Dibujo a) 
    | Juntar Float Float (Dibujo a) (Dibujo a)
    | Encimar (Dibujo a) (Dibujo a)
    deriving (Eq, Show)

-- Agreguen los tipos y definan estas funciones

-- Construcción de dibujo. Abstraen los constructores.

figura :: a -> Dibujo a
figura = Figura

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

apilar :: Float  -> Float  -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a 
juntar = Juntar

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

-- Composición n-veces de una función con sí misma. Componer 0 veces
-- es la función constante, componer 1 vez es aplicar la función 1 vez, etc.
-- Componer negativamente es un error!
comp :: (a -> a) -> Int -> a -> a
comp f 0 a = a
comp f n a | n > 0 =   f (comp f (n-1) a)
           | otherwise = error "No se puede componer negativamente"


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
cuarteto a b c d = (.-.) ((///) a b) ((///) c d)  
-- Junto a las figuras a y b en la parte de arriba donde a está a la izquierda y b a la derecha.
-- Junto a las figuras c y d en la parte de abajo donde c está a la izquierda y d a la derecha.

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a 
encimar4 a = (^^^) a ((^^^) (rotar a) ((^^^) (r180 a) (r270 a)))

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar a =  (.-.) ((///) a (rotar a)) ((///) (r180 a) (r270 a))

-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de Floatro a la lógica
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc (Figura d) = fFig d -- si d es una figura, aplico fFig a d
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc (Rotar d) = fRot (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc d)  -- Si d es una rotacion, aplico foldDib a d para obtener el dibujo y aplico fRot a ese dibujo
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc (Espejar d) = fEsp (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc d) -- lo mismo que rotar
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc (Rot45 d) = fRot45 (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc d) -- lo mismo que rotar
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc (Apilar x y d1 d2) = fApi x y (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc d1) (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc d2) -- si d es una apilacion, aplico foldDib a d1 y d2 para obtener los dibujos y aplico fApi a esos dibujos
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc (Juntar x y d1 d2) = fJun x y (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc d1) (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc d2) -- lo mismo que apilar
foldDib fFig fRot fEsp fRot45 fApi fJun fEnc (Encimar d1 d2) = fEnc (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc d1) (foldDib fFig fRot fEsp fRot45 fApi fJun fEnc d2) -- si d es una encimacion, aplico foldDib a d1 y d2 para obtener los dibujos y aplico fEnc a esos dibujos


-- Demostrar que `mapDib figura = id`
mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib f (Figura a) = f a
mapDib f (Rotar d) = Rotar (mapDib f d) 
mapDib f (Espejar d) = Espejar (mapDib f d) 
mapDib f (Rot45 d) = Rot45 (mapDib f d) 
mapDib f (Apilar x y d1 d2) = Apilar x y (mapDib f d1) (mapDib f d2)
mapDib f (Juntar x y d1 d2) = Juntar x y (mapDib f d1) (mapDib f d2)
mapDib f (Encimar d1 d2) = Encimar (mapDib f d1) (mapDib f d2)

-- Junta todas las figuras básicas de un dibujo.
figuras :: Dibujo a -> [a]
figuras = foldDib (:[]) id id id (\_ _ a b -> a ++ b) (\_ _ a b -> a ++ b) (++)
-- (:[])  haciendo referencia a fFig de foldDib, que cada vez que encuentra una figura, la agrega a una lista con solo esa figura
-- id haciendo referencia a fRot, fEsp y fRot45 de foldDib, que cada vez que encuentra una rotacion, espejado o rotacion de 45, no hace nada.
-- (\_ _ a b -> a ++ b) haciendo referencia a fApi y fJun de foldDib, que cada vez que encuentra una apilacion o una juntacion, concatena las listas de figuras de los dos dibujos
-- (++) haciendo referencia a fEnc de foldDib, que cada vez que encuentra una encimacion, concatena las listas de figuras de los dos dibujos