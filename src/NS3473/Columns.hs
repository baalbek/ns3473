{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}
module NS3473.Columns where

import qualified NS3473.Common as C
import qualified NS3473.Buckling as X
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R

data Column = Column {
        h           :: Double,     -- ^ vegghøyde [mm]
        conc        :: M.Concrete -- ^ Betongkvalitet
    } deriving Show

instance X.Bucklable Column where
    ac c     = undefined
    ic c     = undefined
    fcd      = M.fcd . conc 
    emodulus = M.ee . conc 
    hx c     = undefined
    tx c     = undefined
    calcD    = undefined
    lk c     = undefined
    wt c     = undefined
