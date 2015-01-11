{-# LANGUAGE OverloadedStrings #-}
module UserManagementTests (suite) where

import ProgramExecutor (ProcSpec, withAstralServer)
import RESTClient (get, post)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

suite :: Test.Framework.Test
suite =
    testGroup "User management tests"
    [ testCase "Shall accept a new user" shallAcceptNewUserTest
    ]

shallAcceptNewUserTest :: Assertion
shallAcceptNewUserTest =
    withAstralServer defaultServer $ do
      (code, _) <- get "127.0.0.1" 8888 "/register"
      assertEqual "Shall be equal" 201 code

defaultServer :: ProcSpec
defaultServer = ("./dist/build/astral-mining-server/astral-mining-server"
                , ["--port=8888"])
