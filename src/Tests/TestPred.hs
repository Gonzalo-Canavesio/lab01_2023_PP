{-# LANGUAGE LambdaCase #-}
module Tests.TestPred (
  tests
) where

import Test.HUnit
import Pred
import Dibujo

---- Test the cambiar function
--testCambiar :: Test
--testCambiar = TestList [
--    cambiar (\x -> x == 1) (\x -> cuadrado) [[1, 2], [3, 1]] ~?= [[cuadrado, 2], [3, cuadrado]],
--    cambiar (\x -> x == 'a') (\_ -> figura 'b') [['a', 'b'], ['a', 'c']] ~?= [['b', 'b'], ['b', 'c']]
--  ]

---- Test the anyDib function
--testAnyDib :: Test
--testAnyDib = TestList [
--    anyDib (\x -> x == 1) [[1, 2], [3, 4]] ~?= True,
--    anyDib (\x -> x == 'a') [['b', 'c'], ['d', 'e']] ~?= False,
--    anyDib (\x -> x == 0) [[1, 2], [3, 0]] ~?= True
--  ]

---- Test the allDib function
--testAllDib :: Test
--testAllDib = TestList [
--    allDib (\x -> x == 1) [[1, 1], [1, 1]] ~?= True,
--    allDib (\x -> x == 'a') [['a', 'a'], ['a', 'a']] ~?= True,
--    allDib (\x -> x == 0) [[1, 2], [3, 4]] ~?= False
--  ]

-- Test the andP function
testAndP :: Test
testAndP = TestList [
    andP (\x -> x > 0) (\x -> x < 10) 5 ~?= True,
    andP (\x -> x > 0) (\x -> x < 10) 15 ~?= False
  ]

-- Test the orP function
testOrP :: Test
testOrP = TestList [
    orP (\x -> x == 'a') (\x -> x == 'b') 'a' ~?= True,
    orP (\x -> x == 'a') (\x -> x == 'b') 'c' ~?= False
  ]

-- Combine all tests
tests = TestList [
    --testCambiar
    --testAnyDib
    --,testAllDib
    testAndP
    ,testOrP
  ]

