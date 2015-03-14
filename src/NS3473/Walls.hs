{-# LANGUAGE NamedFieldPuns,RecordWildCards  #-}
module NS3473.Walls where

import qualified NS3473.Common as C
import qualified NS3473.Buckling as X
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R

data WallType = External | Interior deriving (Show,Eq)

data Orientation = Vertical | Horizontal deriving (Show,Eq)

data Wall = 
    Wall {
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
    ----------------- hx --------------
    hx w     = let r = rebars w
                   c' = R.cover r
                   d' = (R.diam . R.rebar) r
                   in (t w) - 2*c' - d'
    tx w     = (t w)
    calcD    = undefined
    lk w     = (h w) * (lkf w)
    wt w     = (1000.0 / (ccVertRod w)) * (steelAreaVertRod w)
    
-- | Diameter of rebar 
rodDiam :: Wall 
           -> Double -- ^ [mm]
rodDiam = R.diam . R.rebar . rebars
           
steelAreaHorizRod :: Wall 
                     -> Double   -- ^ [mm2]
steelAreaHorizRod = R.steelAreaRod . R.horizRebar . rebars
                
steelAreaVertRod :: Wall 
                    -> Double   -- ^ [mm2]
steelAreaVertRod = R.steelAreaRod . R.rebar . rebars

ccVertRod :: Wall 
             -> Double   -- ^ [mm2]
ccVertRod = R.ccVert . rebars

-- | Min steel area rebars, vertical or horizontal, NS 3473 18.5.2
minRebars :: Wall 
             -> Orientation 
             -> Double   -- ^ Number of rebars given rebar diam from wall
minRebars wall ori = reqArea 
    where ftk = M.ftk $ conc wall
          ac | ori == Vertical = X.ac wall
             | otherwise = (t wall) * (h wall)
          f | wtype wall == External = 0.6
            | otherwise = 0.3
          reqArea = ac * f * ftk / C.fsk

-- | Max center distance rebars, vertical or horizontal, NS 3473 18.5.2
maxCcRebars :: Wall 
               -> Orientation 
               -> Double   -- ^ Horizontal center distance of rebars given rebar diam from wall
maxCcRebars wall ori = min 300.0 cc 
    where reqar = minRebars wall ori
          rodarFn | ori == Horizontal = steelAreaHorizRod 
                  | otherwise = steelAreaVertRod 
          distConc | ori == Horizontal = h wall
                   | otherwise = 1000.0
          rebarAmountAdjustment' = rebarAmountAdjustment $ rebars wall
          cc = rebarAmountAdjustment' * distConc * (rodarFn wall) / reqar 


-- | Factor for adjusting the total number
-- of rebar pr length
rebarAmountAdjustment :: R.RebarCollection
                         -> Double 
rebarAmountAdjustment (R.SingleWallRebars _ _ _ _ _) = 1.0
rebarAmountAdjustment (R.DoubleWallRebars _ _ _ _ _) = 2.0
