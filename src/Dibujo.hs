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

data Dibujo a = Figura a | Rotar Dibujo a | Espejar Dibujo a 
    | Rot45 Dibujo a
    | Apilar Float Float Dibujo a Dibujo a 
    | Juntar Float Float Dibujo a Dibujo a
    | Encimar Dibujo a Dibujo a
    deriving (Eq, Show)

-- Agreguen los tipos y definan estas funciones

-- Construcción de dibujo. Abstraen los constructores.

figura :: a -> Dibujo a
figura = Figura

rotar :: a -> Dibujo a
rotar = Rotar

espejar :: a -> Dibujo a
espejar = Espejar

rot45 :: a -> Dibujo a
rot45 = Rot45

apilar :: Float  -> Float  -> Dibujo a -> Dibujo a
apilar = Apilar

juntar :: Float -> Float -> Dibujo a -> Dibujo a 
juntar = Juntar

encimar :: Dibujo a -> Dibujo a 
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
r180 a = comp rotar 2 a

r270 :: Dibujo a -> Dibujo a
r270 a = comp rotar 3 a

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) a b = apilar 0.5 0.5 a b  --nose sie hay que poer 0,5 o algo mas grande  
-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) a b = juntar 0.5 0.5 a b  

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) a b = encimar a b

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = .-. (/// a b) (/// c d)  
-- Junto a las figuras a y b en la parte de arriba donde a está a la izquierda y b a la derecha.
-- Junto a las figuras c y d en la parte de abajo donde c está a la izquierda y d a la derecha.


-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a 
encimar4 a = (^^^) a ((^^^) (rotar a) ((^^^) (r180 a) (r270 a)))

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar = undefined

-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de Floatro a la lógica
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
foldDib = undefined

-- Demostrar que `mapDib figura = id`
mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib = undefined

-- Junta todas las figuras básicas de un dibujo.
figuras = undefined
