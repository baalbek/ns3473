{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit (runTestTT)

import TestRebars (testRebars)
import TestFoundations (testFoundations)
import TestBeams (testBeams)
import TestWalls (testWalls)

main = do
    --runTestTT testRebars
    --runTestTT testFoundations
    --runTestTT testBeams
    runTestTT testWalls
