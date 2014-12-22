{-# LANGUAGE OverloadedStrings #-}

module TestRebars where 

import Test.HUnit (test,assertEqual,(~:))

import NS3473.Common (ro2dec)
import qualified NS3473.Rebars as R

testRebars = test [
                "R1" ~: do 
                    let rebar = R.FloorRebars (R.Rebar 12) 100 25
                    let area = R.totalSteelArea rebar
                    assertEqual "[R1] area" 1131 (ro2dec area 1)
                    let d' = R.ddist rebar
                    assertEqual "[R1] ddist" 31 (ro2dec d' 1)
                    let steelArea1 = R.steelArea rebar 469 
                    assertEqual "[R1] steelArea1" 530.4 (ro2dec steelArea1 1)
                ]

