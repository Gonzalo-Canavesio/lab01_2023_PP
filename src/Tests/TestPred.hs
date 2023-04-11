{-# LANGUAGE LambdaCase #-}
module Tests.TestPred (
  tests
) where

import Test.HUnit
import Pred

test1 = TestCase $ assertEqual "pred" (id 1) 1

tests = TestList [test1]
