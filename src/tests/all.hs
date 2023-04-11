import Test.HUnit
import qualified TestDibujo
import qualified TestPred

allTests = TestList [TestDibujo.tests, TestPred.tests]

main = runTestTT allTests
