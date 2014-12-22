{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}

import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Buckling as X

conc = M.newConc "35" 

rebar = R.FloorRebars (R.Rebar 12) 100 25

