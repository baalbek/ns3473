{-# LANGUAGE OverloadedStrings #-}

module TestWalls where 

import Test.HUnit (test,assertEqual,(~:))

import NS3473.Common (ro2dec)
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Walls as W
import qualified NS3473.Buckling as X


conc = M.newConc "35"

rebar12 = R.Rebar 12

rebar10 = R.Rebar 10

rebar = R.DoubleWallRebars rebar12 100 rebar10 100 40

testWalls = test [
                "W1" ~: do 
                    let wall = W.Wall 200 2400 conc rebar W.External 1.0
                    assertEqual "[W1a] min area horizontal rebars" 1468.8 (ro2dec (W.minRebars wall W.Horizontal) 1)
                    assertEqual "[W1b] min area vertical rebars" 612.0 (ro2dec (W.minRebars wall W.Vertical) 1)
                    assertEqual "[W1c] max cc horizontal rebars" (2*128.35) (ro2dec (W.maxCcRebars wall W.Horizontal) 1)
                    assertEqual "[W1d] max cc vertical rebars" 300.0 (ro2dec (W.maxCcRebars wall W.Vertical) 1)
                    assertEqual "[W1e] wt" 1131.0 (ro2dec (X.wt wall) 1)
                ]
