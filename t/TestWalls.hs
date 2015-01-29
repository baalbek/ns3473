{-# LANGUAGE OverloadedStrings #-}

module TestWalls where 

import Test.HUnit (test,assertEqual,(~:))

import NS3473.Common (ro2dec)
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Walls as W


conc = M.newConc "35"

rebar12 = R.Rebar 12

rebar10 = R.Rebar 10

rebar = R.DoubleWallRebars rebar12 100 rebar10 100 40

testWalls = test [
                "W1" ~: do 
                    let wall = W.Wall 200 2400 conc rebar W.External 1.0
                    assertEqual "[W1a] min cc horizontal rebars" 128.3 (ro2dec (W.minHorizRebars wall) 1)
                    assertEqual "[W1a] min amount vertical rebars" 369.6 (ro2dec (W.minVerticalRebars wall) 1)
                ]
