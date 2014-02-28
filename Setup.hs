--
-- Setup
--

import Distribution.Simple
import Distribution.Simple.UserHooks
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.Cmd
import System.FilePath

main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks {
              runTests = runTests'
            }

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = system testprog >> return ()
  where
    testprog = buildDir lbi "test" "test"
