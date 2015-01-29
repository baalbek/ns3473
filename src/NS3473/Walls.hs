{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}
module NS3473.Walls where

import qualified NS3473.Common as C
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
    calcD    = undefined
    lk w     = (h w) * (lkf w)
    wt w     = 0.02 
    
-- | Diameter of rebar 
rodDiam :: Wall 
           -> Double -- ^ [mm]
rodDiam = R.diam . R.rebar . rebars
           
steelAreaHorizRod :: R.RebarCollection 
                -> Double   -- ^ [mm2]
steelAreaHorizRod = R.steelAreaRod . R.horizRebar 
                
steelAreaVertRod :: R.RebarCollection 
                    -> Double   -- ^ [mm2]
steelAreaVertRod = R.steelAreaRod . R.vertRebar 

minRebars :: Wall 
             -> Double   -- ^ Factor due to wall type etc
             -> Double   -- ^ Concrete section area [mm2]
             -> (R.RebarCollection -> Double) -- ^ Steel area rod function
             -> Double   -- ^ Number of rebars given rebar diam from wall
minRebars wall f ac rodAreaFn = reqArea / rar
    where rar = rodAreaFn $ rebars wall
          ftk = M.ftk $ conc wall
          reqArea = ac * f * ftk / C.fsk
          -- reqArea = (X.ac wall) * f * ftk / C.fsk

-- | Minimum horiozontal rebars, NS 3473 18.5.2
minHorizRebars :: Wall 
                  -> Double   -- ^ Horizontal center distance of rebars given rebar diam from wall
minHorizRebars wall = (h wall) / (minRebars wall f ac steelAreaHorizRod)
    where f | wtype wall == External = 0.6
            | otherwise = 0.3
          ac = (t wall) * (h wall)

-- | Minimum vertical rebars, NS 3473 18.5.2
minVerticalRebars :: Wall
                     -> Double   -- ^ Vertical center distance of rebars given rebar diam from wall
minVerticalRebars wall = 1000.0 / (minRebars wall 0.3 ac steelAreaVertRod)
    where ac = X.ac wall
