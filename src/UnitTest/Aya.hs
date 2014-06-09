--
-- UnitTest.Aya: test suites of Sudoku modules
--

module UnitTest.Aya where

import Test.Framework
import qualified UnitTest.Aya.Algebra   as Algebra   (testSuite)
import qualified UnitTest.Aya.Color     as Color     (testSuite)
import qualified UnitTest.Aya.Geometry  as Geometry  (testSuite)
import qualified UnitTest.Aya.InitWorld as InitWorld (testSuite)
import qualified UnitTest.Aya.Material  as Material  (testSuite)
import qualified UnitTest.Aya.Object    as Object    (testSuite)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite = foldl (++) [] [
    Algebra.testSuite
  , Color.testSuite
  , Geometry.testSuite
  , InitWorld.testSuite
  , Material.testSuite
  , Object.testSuite
  ]
