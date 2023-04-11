{-# LANGUAGE LambdaCase #-}
module Tests.TestDibujo (
  tests
) where


import Test.HUnit
import Dibujo

test1 = TestCase $ assertEqual "dibujo" (id 1) 1

tests = TestList [test1]
