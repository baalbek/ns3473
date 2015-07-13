{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}

import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Beams as B

conc = M.newConc "35" 
r = R.SingleRowBeamRebars (R.Rebar 12) 4 25
links = B.Link 8

b = B.TProfile 300 550 conc r (B.Link 8) 400 250 
