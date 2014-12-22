{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}
module NS3473.Foundations where

import qualified NS3473.Concrete as M
import qualified NS3473.Utils as U
import qualified NS3473.Common as C
import qualified NS3473.Rebars as R

data FoundationResult = OneFr Double | TwoFr Double Double 
        deriving Show

data Foundation = 
        SquareFoundation {
            fundH :: Double,                    -- ^ Fundamenthoyde [mm]
            fundB :: Double,                    -- ^ Fundamentbredde [mm]
            col :: Double,                  -- ^ Søylebredde [mm]
            conc :: M.Concrete,             -- ^ Betong
            rebars :: R.RebarCollection     -- ^ Armering
        }
        | WallFoundation {
            fundH :: Double,                    -- ^ Fundamenthoyde [mm]
            fundB :: Double,                    -- ^ Fundamentbredde [mm]
            wall :: Double,                 -- ^ Veggtykkelse [mm]
            conc :: M.Concrete,             -- ^ Betong
            rebars :: R.RebarCollection     -- ^ Armering
        }
        | RectFoundation {
            fundH :: Double,                    -- ^ Fundamenthoyde [mm]
            fundB1 :: Double,                   -- ^ Korteste fundamentbredde [mm]
            fundB2 :: Double,                   -- ^ Lengste fundamentbredde [mm]
            col1 :: Double,                 -- ^ Korteste søylebredde [mm]
            col2 :: Double,                 -- ^ Lengste søylebredde [mm]
            conc :: M.Concrete,             -- ^ Betong
            rebars :: R.RebarCollection     -- ^ Armering
        }
        deriving Show

----------------------------------------------------------------------------------------
------------------------------------ Skjær ---------------------------------------------
----------------------------------------------------------------------------------------
shearArea' :: Double      -- ^ d 
              -> Double   -- ^ Foundation width 
              -> Double   -- ^ Column width
              -> Double                -- ^ [mm2]
shearArea' d' b' col' = laverage * l2
    where l1 = col' + d'
          l2 = (b' - col') / 2.0
          laverage = (l1 + b') / 2.0

-- | Calculates area of foundation that contributes to shear 
-- each foundation side. For the square/rectangular
-- foundations, this is a trapezoid area.
-- For RectFoundation, returns area for fundB1 in the first result, and fundB2 in the last.
shearArea :: Foundation 
             ->  FoundationResult -- ^ [mm2]
shearArea WallFoundation { fundB,wall } = OneFr (1000 * (fundB - wall) / 2.0)
shearArea fund@SquareFoundation { fundB,col }  = OneFr $ shearArea' (calcD fund) fundB col
shearArea fund@RectFoundation { fundB1,fundB2,col1,col2 }  = 
    let area1 = shearArea' d' fundB1 col1
        area2 = shearArea' d' fundB2 col2
        d' = calcD fund
    in TwoFr area1 area2 

-- | dimensjonerende skjær v d/2 fra soyleliv
dimShear :: Foundation 
           -> C.Load           -- ^ Jevnt fordelt på grunn, q [kN/m2]
           -> FoundationResult -- ^ [kNm]
dimShear fund@(RectFoundation _ _ _ _ _ _ _) q = TwoFr (area1 * q') (area2 * q')
    where TwoFr area1 area2 = shearArea fund 
          q' = q / 1000000.0
dimShear fund q = OneFr $ area * q / 1000000.0
    where OneFr area = shearArea fund

-- | Areal av betongen som må bære skjærkraften
-- i kritisk snitt d/2 fra søyle-/veggliv
shearAreaConc :: Foundation 
                 -> FoundationResult -- ^ [mm2]
shearAreaConc fund@SquareFoundation { fundH,col } = OneFr $ fundH * (bw fund) -- ((calcD fund) + col)

-- | Bredde av betongen som bærer skjærkraften i kritisk 
-- snitt d/2 fra søyle-/veggliv.
-- RectFoundation minste bredde. For største bredde RectFoundation
-- bruk bw2
bw :: Foundation 
      -> Double -- ^ [mm2]
bw fund@WallFoundation { wall } = ((calcD fund) + wall)
bw fund@SquareFoundation { col } = ((calcD fund) + col)
bw fund@RectFoundation { col1 } = ((calcD fund) + col1)

bw2 :: Foundation 
       -> Double -- ^ [mm2]
bw2 fund@RectFoundation { col2 } = ((calcD fund) + col2)

-- | kv høydefaktor i vcd beregning
kv :: Foundation
      -> Double
kv fund | kv' < 1.0 = 1.0  
        | kv' > 1.4 = 1.4
        | otherwise = kv'
    where kv' = (1500 - (fundH fund)) / 1000.0

-- | 0.33 vcd leddet
vcd1 :: Foundation
        -> Double -- ^ kN
vcd1 fund  = (0.33 * (ftd' + f1) * bw' * kv' * d) / 1000.0
    where ftd' = M.ftd $ conc fund
          ka = 100.0
          kv' = kv fund
          d = calcD fund
          bw' = (bw fund)
          f1 = (ka * steel) / (C.gamc * bw' * d)
          steel = R.steelArea (rebars fund) bw'
    

-- | 0.66  vcd leddet
vcd2 :: Foundation
        -> Double -- ^ kN
vcd2 fund = 0.66 * (ftd' * d * bw' * kv') / 1000.0
    where ftd' = M.ftd $ conc fund
          kv' = kv fund
          d = calcD fund
          bw' = (bw fund)

-- | NS 3473 12.3.2.1, kapasitet for strekkbrudd. 
-- For WallFoundation kapasitet pr 1000.0 mm
vcd :: Foundation 
       -> FoundationResult -- ^ [kN]
vcd fund = OneFr $ min (vcd1 fund) (vcd2 fund)

shearAs' :: Double     -- ^ Søylebredde
            -> Double  -- ^ d
            -> Double  -- ^ Skjærkraft [kN]
            -> Double  -- ^ [mm2] 
shearAs' bw' d v = 1.11 * v * bw' * 1000 / (d * C.fsd * (sqrt 2))

-- | Skjærarmeringsareal for skjærkraft v 
shearAs :: Foundation 
           -> Double  -- ^ Skjærkraft [kN]
           -> FoundationResult  -- ^ [mm2] 
shearAs fund@SquareFoundation { col } v = OneFr $ shearAs' col (calcD fund)  v 
shearAs fund@WallFoundation { wall } v = OneFr $ shearAs' wall (calcD fund) v 

vccd' :: Double    -- ^ fcd
         -> Double -- ^ bw
         -> Double -- ^ d
         -> Double -- ^ [kN]
vccd' fcd' bw' d' = result / 1000.0 
    where vccd1 = 0.25 * fcd' * bw' * 0.9 * d' * (1 + cot)
          cot = 1 / (tan (C.toRadians 45))
          vccd2 = 0.45 * fcd' * bw' * 0.9 * d' 
          result = min vccd1 vccd2 

-- | NS 3473 12.3.2.5, kapasitet for trykkbrukk.
vccd :: Foundation 
        -> FoundationResult -- ^ [kN]
vccd fund = OneFr $ vccd' (M.fcd $ conc fund) (bw fund) (calcD fund)  
   
----------------------------------------------------------------------------------------
----------------------------------- Moment ---------------------------------------------
----------------------------------------------------------------------------------------

-- | dimensjonerende moment v /soyleliv
dimMoment :: Foundation 
             -> C.Load            -- ^ Jevnt fordelt på grunn, q [kN/m2]
             -> FoundationResult  -- ^ [kNm]
dimMoment SquareFoundation { fundB,col } q = OneFr $ b' * l * l * q / 2.0
    where l = (fundB - col) / 2000.0
          b' = fundB / 1000.0

mcd' :: M.Concrete   
        -> Double   -- ^ Width [mm]
        -> Double   -- ^ d [mm] 
        -> Double   -- ^ [kNm]
mcd' conc w d = mr' * w * d * d * fcd' / 1000000.0
    where mr' = M.mr conc 
          fcd' = M.fcd conc

-- | Betongtverrsnittest momentkapasitet
mcd :: Foundation 
       -> FoundationResult  -- ^ [kNm]
mcd RectFoundation { fundH,fundB1,fundB2,conc,rebars } = undefined
mcd fund = OneFr $ mcd' (conc fund) (fundB fund) (calcD fund)
       
mfAs :: Foundation 
        -> C.StaticMoment   -- ^ [kNm]
        -> FoundationResult -- ^ [mm2]
mfAs fund@RectFoundation {..} m = undefined
mfAs fund m = OneFr $ m' / (C.fsd * z) 
    where OneFr z = calcZ fund m
          m' = m * 1000000.0

----------------------------------------------------------------------------------------
---------------------------------------- Diverse --------------------------------------- 
----------------------------------------------------------------------------------------

-- | Calc distance d form top concrete to centroid rebars area
calcD :: Foundation 
         -> Double       -- ^ [mm]
calcD fund = (fundH fund) - d' 
    where d' = R.ddist (rebars fund)

calcZ :: Foundation
         -> C.StaticMoment   -- ^ [kNm] 
         -> FoundationResult -- ^ [mm]
calcZ fund@RectFoundation { conc } m = undefined
calcZ fund m = OneFr $ (1.0 - (alphad'*f1)) * (calcD fund)
    where alphad' = M.alphad $ conc fund
          OneFr mcd' =  mcd fund
          f1 = m / mcd'
          
