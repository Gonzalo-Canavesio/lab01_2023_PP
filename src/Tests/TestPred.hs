{-# LANGUAGE LambdaCase #-}
module Tests.TestPred (
  tests
) where

import Test.HUnit
import Pred
import Dibujo

-- funcion auxiliar de prueba, para simplificar las cosas representa un rectangulo con dos coordenadas
-- la primera es la esquina inferior izquierda (left bottom)
-- la segunda es la esquina superior derecha (right top)
rectangulo :: (Num a, Ord a) => (a, a) -> (a, a) -> Dibujo ((a,a), (a,a))
rectangulo lb rt = figura (lb, rt)

-- Test the andP function
testAndP :: Test
testAndP = TestLabel "testAndP" $ TestList [
    andP (> 0) (< 10) 5 ~?= True,
    andP (> 0) (< 10) 15 ~?= False
  ]

-- Test the orP function
testOrP :: Test
testOrP = TestLabel "testOrP" $ TestList [
    orP (== 'a') (== 'b') 'a' ~?= True,
    orP (== 'a') (== 'b') 'c' ~?= False
  ]

-- Test the cambiar function
testCambiar :: Test
testCambiar = TestLabel "testCambiar" $ TestList [
    cambiar (const True) (\_ -> rectangulo (0, 0) (2, 2)) (rectangulo (0, 0) (1, 1)) ~?= rectangulo (0, 0) (2, 2),
    cambiar (const False) (\_ -> rectangulo (0, 0) (1, 1)) (rectangulo (0, 0) (1, 1)) ~?= rectangulo (0, 0) (1, 1),
    cambiar (\(lb, rt) -> lb == (0,0)) (\_ -> rectangulo (1, 1) (2, 2)) (rectangulo (0, 0) (1, 1)) ~?= rectangulo (1, 1) (2, 2),
    cambiar (\(lb, rt) -> rt == (1,1)) (\_ -> rectangulo (2, 2) (3, 3)) (rectangulo (0, 0) (1, 1)) ~?= rectangulo (2, 2) (3, 3)
  ]

-- Test the anyDib function
testAnyDib :: Test
testAnyDib = TestLabel "testAnyDib" $ TestList [
    anyDib (const True) (rectangulo (0, 0) (1, 1)) ~?= True,
    anyDib (const False) (rectangulo (0, 0) (1, 1)) ~?= False,
    anyDib (\(lb, rt) -> lb == (0,0)) (rectangulo (0, 0) (1, 1)) ~?= True,
    anyDib (\(lb, rt) -> lb == (1,1)) (rectangulo (0, 0) (1, 1)) ~?= False
  ]

-- Test the allDib function
testAllDib :: Test
testAllDib = TestLabel "testAllDib" $ TestList [
    allDib (const True) (rectangulo (0, 0) (1, 1)) ~?= True,
    allDib (const False) (rectangulo (0, 0) (1, 1)) ~?= False,
    allDib (\(lb, rt) -> lb == (0,0)) (rectangulo (0, 0) (1, 1)) ~?= True,
    allDib (\(lb, rt) -> lb == (1,1)) (rectangulo (0, 0) (1, 1)) ~?= False
  ]

-- Combine all tests into a single test list
tests :: Test
tests = TestList [
    testCambiar
    ,testAnyDib
    ,testAllDib
    ,testAndP
    ,testOrP
    ]
