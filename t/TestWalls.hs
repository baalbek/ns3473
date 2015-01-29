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
                    assertEqual "[W1a] min area horizontal rebars" 1468.8 (ro2dec (W.minRebars wall W.Horizontal) 1)
                    assertEqual "[W1b] min area vertical rebars" 306.0 (ro2dec (W.minRebars wall W.Vertical) 1)
                    assertEqual "[W1c] max cc horizontal rebars" 128.3 (ro2dec (W.maxCcRebars wall W.Horizontal) 1)
                    assertEqual "[W1c] max cc vertical rebars" 369.6 (ro2dec (W.maxCcRebars wall W.Vertical) 1)
                ]
