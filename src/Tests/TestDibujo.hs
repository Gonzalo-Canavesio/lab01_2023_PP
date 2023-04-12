{-# LANGUAGE LambdaCase #-}
module Tests.TestDibujo (
  tests
) where


import Test.HUnit
import Dibujo
import Internals.Dibujo

testDibujo = Figura "X"

-- Test that rotating a Dibujo 180 degrees twice returns the original Dibujo
testRot180 = TestCase $ assertEqual "Rotation 180 degrees twice" expected result
  where
    expected = testDibujo
    result = r180 $ r180 testDibujo

-- Test that rotating a Dibujo 270 degrees four times returns the original Dibujo
testRot270 = 
  TestCase $ assertEqual "Rotation 270 degrees four times" expected result
  where
    expected = testDibujo
    result = r270 $ r270 $ r270 $ r270 testDibujo

-- Test that stacking two Dibujos vertically using (.-.) returns a Dibujo with the expected height and width
testApilar = 
  TestCase $ assertEqual "Apilar two Dibujos" expected result
  where
    expected = Apilar 1.0 1.0 testDibujo testDibujo
    result = testDibujo .-. testDibujo

-- Test that joining two Dibujos horizontally using (///) returns a Dibujo with the expected height and width
testJuntar = 
  TestCase $ assertEqual "Juntar two Dibujos" expected result
  where
    expected = Juntar 1.0 1.0 testDibujo testDibujo
    result = testDibujo /// testDibujo

-- Test that superimposing two Dibujos using (^^^) returns a Dibujo with the expected height and width
testEncimar = 
  TestCase $ assertEqual "Encimar two Dibujos" expected result
  where
    expected = Encimar testDibujo testDibujo
    result = testDibujo ^^^ testDibujo

-- Test that creating a cuarteto with four Dibujos returns a Dibujo with the expected height and width
testCuarteto = 
  TestCase $ assertEqual "Create cuarteto with four Dibujos" expected result
  where
    expected = Apilar 1.0 1.0 (Juntar 1.0 1.0 testDibujo testDibujo) (Juntar 1.0 1.0 testDibujo testDibujo)
    result = cuarteto testDibujo testDibujo testDibujo testDibujo

-- Test that superimposing a Dibujo with its four rotations using encimar4 returns a Dibujo with the expected height and width
testEncimar4 = 
  TestCase $ assertEqual "Superimpose a Dibujo with its four rotations using encimar4" expected result
  where
    expected = Encimar (r270 testDibujo) (Encimar (r180 testDibujo) (Encimar (rotar testDibujo) testDibujo))
    result = encimar4 testDibujo

-- Test that creating a cyclic Dibujo with a Dibujo returns a Dibujo with the expected height and width
testCiclar = TestCase $ assertEqual "Create cyclic Dibujo with a Dibujo" expected result
  where
    expected = Apilar 1.0 1.0 (Juntar 1.0 1.0 (rotar testDibujo) testDibujo) (Juntar 1.0 1.0 (r180 testDibujo) (r270 testDibujo))
    result = ciclar testDibujo

tests = TestList [
   testRot180
  ,testRot270
  ,testApilar
  ,testJuntar
  ,testEncimar
  ,testCuarteto
  ,testEncimar4
  ,testCiclar
  ]
