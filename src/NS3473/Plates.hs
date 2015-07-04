{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards  #-}

module NS3473.Plates where

import qualified NS3473.Common as C
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R

data Plate = 
    Plate {
        plateH :: Double,                  -- ^ Tverrsnittshoyde
        conc   :: M.Concrete,         -- ^ Betong
        rebars :: R.RebarCollection   -- ^ Armering
    } deriving Show

----------------------------------------------------------------------------------------
---------------------------------------- Shear ----------------------------------------- 
----------------------------------------------------------------------------------------
-- | Betongens skjærkapasitet, NS3473 12.3.2.1
vcd :: Plate 
       -> Double  -- ^ [kN]
vcd plate = min res1 res2
    where myd = calcD plate 
          kw = min 1.4 (max 1.0 (1.5 - myd / 1000.0))
          ftd' = M.ftd $ conc plate
          ka = 100.0
          bw = 1000.0 -- (beamW beam)
          as = R.totalSteelArea $ rebars plate
          k0 = (ka * as) / (C.gamc * bw * myd)
          res1 = 0.33 * (ftd' + k0) * bw *  myd * kw / 1000.0
          res2 = 0.66 * ftd' * bw * myd * kw / 1000.0

-- | Calc distance d form top concrete to centroid rebars area
calcD :: Plate 
         -> Double       -- ^ [mm]
calcD plate = (plateH plate) - d' 
    where d' = R.ddist (rebars plate)

