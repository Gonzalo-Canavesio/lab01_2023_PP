module Internals.Dibujo where

{-
--Gramática de las figuras:
<Fig> ::= Figura <Bas> | Rotar <Fig> | Espejar <Fig> | Rot45 <Fig>
    | Apilar <Float> <Float> <Fig> <Fig> 
    | Juntar <Float> <Float> <Fig> <Fig> 
    | Encimar <Fig> <Fig>
-}

{- Se agregó al lenguaje el operador Modificar para permitir modificar las 
proporciones de un dibujo -}
data Dibujo a = Figura a | Rotar (Dibujo a) | Espejar (Dibujo a) 
    | Rot45 (Dibujo a)
    | Modificar Float Float (Dibujo a)
    | Apilar Float Float (Dibujo a) (Dibujo a) 
    | Juntar Float Float (Dibujo a) (Dibujo a)
    | Encimar (Dibujo a) (Dibujo a)
    deriving (Eq, Show)

-- Funciones para abstraer los constructores de la construción del dibujo
figura :: a -> Dibujo a
figura = Figura

rotar :: Dibujo a -> Dibujo a
rotar d = case d of
  Rotar (Rotar (Rotar a)) -> a
  _ -> Rotar d 
-- Si se aplica una 4ta rotación seguida, solo se remueven las 3 rotaciones

modificar :: Float -> Float -> Dibujo a -> Dibujo a
modificar = Modificar

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
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
comp f n a | n > 0 =   f (comp f (n - 1) a)
           | otherwise = error "No se puede componer negativamente"
