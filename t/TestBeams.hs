{-# LANGUAGE OverloadedStrings #-}

module TestBeams where 

import Test.HUnit (test,assertEqual,(~:))

import NS3473.Common (ro2dec)
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Beams as B

conc = M.newConc "35" 

testBeams = test [
                "B1" ~: do 
                    let rebars = R.SingleRowBeamRebars (R.Rebar 12) 4 25
                    let links = B.Link 8 
                    let b = B.RectBeam 200 500 conc rebars links
                    assertEqual "[B1] calcD" 469.0 (ro2dec (B.calcD b) 1)
                    assertEqual "[B1] calcZ" 429.0 (ro2dec (B.calcZ b 100) 1)
                    assertEqual "[B1] mcd" 193.6 (ro2dec (B.mcd b) 1)
                    assertEqual "[B1] as links" 100.5 (ro2dec (B.asLink (B.links b)) 1)
                    let Just ccLinks = B.ccLinks b 100 90.4
                    assertEqual "[B1] cc links" 190.8 (ro2dec ccLinks 1)
                    assertEqual "[B1] min as links" 204.0 (ro2dec (B.minAsLinks b) 1)
                    -- assertEqual "[B1] min cc links" 492.8 (ro2dec (B.minCcLinks b) 1)
                    assertEqual "[B1] min cc links" 262.8 (ro2dec (B.minCcLinks b) 1)
                ]
