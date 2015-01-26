{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}
module NS3473.Walls where

import qualified NS3473.Buckling as X
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R

data WallType = External | Interior deriving (Show,Eq)

data Wall = Wall {
                t           :: Double,     -- ^ veggtykkelse [mm]
                h           :: Double,     -- ^ vegghøyde [mm]
                conc        :: M.Concrete, -- ^ Betongkvalitet
                rebars      :: R.RebarCollection,    -- ^ Armering
                wtype       :: WallType,   -- ^ Inner/yttervegg
                lkf         :: Double      -- ^ Knekklengdefaktor
            }
        deriving Show

instance X.Bucklable Wall where
    ac w     = (t w) * 1000.0 
    ic w     = 1000.0 * ((t w) ** 3) / 12.0
    fcd      = M.fcd . conc 
    emodulus = M.ee . conc 
    hx w     = let r = rebars w
                   c' = R.cover r
                   d' = (R.diam . R.rebar) r
                   in (t w) - 2*c' - d'
    tx w     = (t w)
    calcD  = undefined
    lk w   = (h w) * (lkf w)
    wt w = 0.02 
    
