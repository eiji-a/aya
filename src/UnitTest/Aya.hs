--
-- UnitTest.Aya: test suites of Sudoku modules
--

module UnitTest.Aya where

import Test.Framework
import qualified UnitTest.Aya.Algebra   as Algebra   (testSuite)
import qualified UnitTest.Aya.Geometry  as Geometry  (testSuite)
import qualified UnitTest.Aya.InitWorld as InitWorld (testSuite)
import qualified UnitTest.Aya.Object    as Object    (testSuite)
import qualified UnitTest.Aya.Physics   as Physics   (testSuite)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite = foldl (++) [] [
    Algebra.testSuite
  , Geometry.testSuite
  , InitWorld.testSuite
  , Object.testSuite
  , Physics.testSuite
  ]
