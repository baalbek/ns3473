{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}

import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Beams as B
import qualified NS3473.Cracking as CR


conc = M.newConc "35" 
rebar = R.SingleRowBeamRebars (R.Rebar 12) 4 25
links = B.Link 8

b = B.RectBeam 750 350 conc rebar links

