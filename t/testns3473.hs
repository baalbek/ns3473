{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit (runTestTT)

import TestRebars (testRebars)
import TestFoundations (testFoundations)
import TestBeams (testBeams)

main = do
    runTestTT testRebars
    runTestTT testFoundations
    runTestTT testBeams
