--
-- Test: top level of test suites
--

import System.Environment (getArgs)
import Test.Framework

import qualified UnitTest.Sudoku as Sudoku (testSuite)

main :: IO ()
main = runTests

runTests :: IO ()
runTests =
  do
    args <- filter (/= "--tests") `fmap` getArgs
    (defaultMainWithArgs testSuite args)

testSuite :: [Test]
testSuite = foldl (++) [] [
    Sudoku.testSuite
  ]


