module OptCrackerTests (suite) where

import OptCracker (crack)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

suite :: Test.Framework.Test
suite =
    testGroup "OptCracker tests" 
    [ testCase "Shall tell explicit help if -h" $
               shallTellExplicitHelpTest ["-h"]
    , testCase "Shall tell explicit help if -?" $
               shallTellExplicitHelpTest ["-?"]
    , testCase "Shall tell explicit help if --help" $
               shallTellExplicitHelpTest ["--help"]
    , testCase "Shall tell explicit help if no opts" $
               shallTellExplicitHelpTest []
    , testCase "Unknown option shall tell error" $
               shallTellErrorTest ["-k"]
    , testCase "Unknown option shall tell error also if -h" $
               shallTellErrorTest ["-h", "-k"]
    , testCase "Shall tell port 42" $
               shallTellPortTest ["-p", "42"] 42
    , testCase "Shall tell port 42" $
               shallTellPortTest ["--port=42"] 42
    ]

shallTellExplicitHelpTest :: [String] -> Assertion
shallTellExplicitHelpTest opts =
    assertEqual "Shall be equal" (Right (True, Nothing)) $ crack opts

shallTellErrorTest :: [String] -> Assertion
shallTellErrorTest opts =
    case crack opts of
      Right _ -> assertBool "Shall be Left _" False
      _       -> return ()

shallTellPortTest :: [String] -> Int -> Assertion
shallTellPortTest opts expectedPort =
    assertEqual "Shall be equal" (Right (False, Just expectedPort)) $ crack opts
 
