module Main (main) where

import Test.Framework (Test, defaultMain)
import qualified OptCrackerTests as OptCrackerTests
import qualified UserManagementTests as UserManagementTests

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite = [ OptCrackerTests.suite
            , UserManagementTests.suite ]
