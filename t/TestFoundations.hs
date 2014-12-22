{-# LANGUAGE OverloadedStrings #-}

module TestFoundations where 

import Test.HUnit (test,assertEqual,(~:))

import NS3473.Common (ro2dec)
import qualified NS3473.Concrete as M
import qualified NS3473.Foundations as F
import qualified NS3473.Rebars as R

conc = M.newConc "35" 

rebar = R.FloorRebars (R.Rebar 12) 100 25

testFoundations = test [
                "F1" ~: do 
                    let r = F.SquareFoundation 200 2600 300 conc rebar
                    let F.OneFr m = F.dimMoment r 50
                    assertEqual "[F1] dimMoment" 86.0 (ro2dec m 1)
                    assertEqual "[F1] calcD" 169.0 (ro2dec (F.calcD r) 1)
                    let F.OneFr shearArea = F.shearArea r 
                    assertEqual "[F1] shearArea" 1764675.0 (ro2dec shearArea 1)
                    let F.OneFr v = F.dimShear r 50 
                    assertEqual "[F1] dimShear" 88.2 (ro2dec v 1)
                    let F.OneFr car = F.shearAreaConc r
                    assertEqual "[F1] shearAreaConc" 93800.0 (ro2dec car 1)
                    assertEqual "[F1] kv" 1.3 (ro2dec (F.kv r) 1)
                    assertEqual "[F1] vcd1" 57.4 (ro2dec (F.vcd1 r) 1)
                    assertEqual "[F1] vcd2" 82.3 (ro2dec (F.vcd2 r) 1)
                    let F.OneFr shas = F.shearAs r 100 
                    assertEqual "[F1] shearAs" 348.3 (ro2dec shas 1)
                    let F.OneFr vccd = F.vccd r
                    assertEqual "[F1] vccd" 513.6 (ro2dec vccd 1)
                    let F.OneFr mcd = F.mcd r
                    assertEqual "[F1] mcd" 326.7 (ro2dec mcd 1)
                    let F.OneFr z = F.calcZ r 100 
                    assertEqual "[F1] z" 160.5 (ro2dec z 1)
                    let F.OneFr mfAs = F.mfAs r 100
                    assertEqual "[F1] mfAs" 1558.0 (ro2dec mfAs 1)
                ]

