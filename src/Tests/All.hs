module Tests.All where
import Test.HUnit
import qualified Tests.TestDibujo as TestDibujo
import qualified Tests.TestPred as TestPred

allTests = TestList [TestDibujo.tests, TestPred.tests]

main = runTestTT allTests
