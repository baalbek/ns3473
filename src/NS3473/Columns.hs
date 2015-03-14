{-# LANGUAGE CPP  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module NS3473.Columns where

import qualified NS3473.Common as CO
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Buckling as X

data Column = 
    Column {
        h1          :: Double,            -- ^ Minste søylebredde [mm]
        h2          :: Double,            -- ^ Største søylebredde [mm]
        cln         :: Double,            -- ^ Søylelengde
        lkf         :: Double,            -- ^ Knekklengdefaktor
        conc        :: M.Concrete,        -- ^ Betongkvalitet
        rebars      :: R.RebarCollection  -- ^ Armering
    } deriving Show


instance X.Bucklable Column where
    ac Column { h1,h2 } = h1*h2
    ic Column { h1,h2 } = h2 * (h1**3) / 12.0
    fcd      = M.fcd . conc 
    emodulus = M.ee . conc 
    hx Column { h1,rebars } = h1 - (2*(R.ddist rebars))
    tx col     = h1 col
    calcD col  = undefined
    lk Column { cln,lkf } = cln*lkf
    wt col = let ac' = X.ac col 
                 as' = 2 * R.totalSteelArea (rebars col) 
                 fcd = (M.fcd . conc) col  in
                 as' * CO.fsd / (ac' * fcd)

    
