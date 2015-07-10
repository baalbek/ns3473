{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}
module NS3473.Beams where

import qualified NS3473.Common as C
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R

newtype Link = Link { diam :: Double } deriving Show

type EeFn = (M.Concrete -> Double)

data DeflectionContext = 
    DeflectionContext { 
        xi :: Double,       -- ^ Factor calculated from een, rho and xiFactor -> look-up in table
        beamLen :: Double,  -- ^ Beam span width [mm]
        u2s :: Double,      -- ^ Ultimate limit to service limit factor
        eeFn :: EeFn 
    } -- deriving Show
        

        

data Beam = 
    RectBeam {
        beamW  :: Double,               -- ^ Tverrsnittsbredde
        beamH  :: Double,               -- ^ Tverrsnittshoyde
        conc   :: M.Concrete,           -- ^ Betong
        rebars :: R.RebarCollection,    -- ^ Armering
        links  :: Link                  -- ^ Bøylearmering
    } deriving Show


-- | Standard beam with C35 concrete, and 8 mm links
defaultBeam :: Double     -- ^ Bjelkebredde   [mm]
               -> Double  -- ^ Bjelkehøyde    [mm]
               -> Double  -- ^ Strekkarmering [mm]
               -> Double  -- ^ Antall strekkstenger [stk]
               -> Beam
defaultBeam w h d numRebars = RectBeam w h myConc myRebar (Link 8)
    where myRebar = R.SingleRowBeamRebars (R.Rebar d) numRebars 25
          myConc = M.newConc "35" 


----------------------------------------------------------------------------------------
------------------------------------- Deflections -------------------------------------- 
----------------------------------------------------------------------------------------
--
-- | Ratio of emodulus of concrete to emodulus of steel
een :: EeFn -> Beam -> Double
een ee RectBeam { conc } = C.esk / (ee conc) 

-- | Ratio of concrete area to steel area of tensile rebars
rho :: Beam -> Double
rho beam = let ac = (beamW beam) * (calcD beam) 
               steel = R.totalSteelArea (rebars beam) in
            steel / ac

-- | een * rho
xiFact :: EeFn
          -> Beam 
          -> Double 
xiFact ee beam = (een ee beam) * (rho beam) 

-- | Calculate xi for xiFact < 0.1
xi1 :: Double -> Double
xi1 x = ((-909.0909)*x**3) + (176.1364*x**2) + ((-13.4129)*x) + 1.0162
     
-- | Calculate xi for xiFact >= 0.1
xi2 :: Double -> Double
xi2 x = ((-4.2559)*x**3) + (5.0684*x**2) + ((-2.4004)*x) + 0.76505

--calcXi :: Double -> Double    
--calcXi x = ((-7.7778)*x**3) + (7.50002*x**2) + ((-2.89365)*x) + 0.79357 
    
-- | EI for calculating deflections
{-
ei2 :: Beam 
      -> DeflectionContext
      -> Double 
ei2 beam DeflectionContext { xi } = 
    let steel = R.totalSteelArea (rebars beam)
        d = calcD beam in 
    C.esk * steel * d * d * xi 
-}

ei :: Beam 
      -> DeflectionContext
      -> Double 
ei beam  DeflectionContext { eeFn } = 
    let steel = R.totalSteelArea (rebars beam)
        d = calcD beam 
        xif = xiFact eeFn beam
        calcXi | xif < 0.1 = xi1
               | otherwise = xi2
        curXi = calcXi xif in 
    C.esk * steel * d * d * curXi 

deflection :: Beam 
              -> DeflectionContext
              -> C.StaticMoment           -- ^ Moment [kNm]
              -> Double
deflection beam ctx m = 
    let m' = m * 1000000.0 
        bl = beamLen ctx in
    m' * bl * bl / (9.6 * (ei beam ctx))
    

---------------------------------------------------------------------------------------
---------------------------------------- Moment ---------------------------------------- 
----------------------------------------------------------------------------------------
-- | Betongtverrsnittest momentkapasitet
mcd :: Beam 
       -> Double -- ^ [kNm]
mcd beam = M.mcd (conc beam) (beamW beam) (calcD beam)

-- | Dimensjonerende armering for boyemoment
mfAs :: Beam 
        -> C.StaticMoment  -- ^ [kNm]
        -> Double          -- ^ [mm2]
mfAs beam m = normM / (z * C.fsd)  
    where z = calcZ beam m 
          normM = m * 10**6
 
----------------------------------------------------------------------------------------
---------------------------------------- Shear ----------------------------------------- 
----------------------------------------------------------------------------------------
-- | Betongens skjærkapasitet, NS3473 12.3.2.1
vcd :: Beam 
       -> Double  -- ^ [kN]
vcd beam = min res1 res2
    where myd = calcD beam
          kw = min 1.4 (max 1.0 (1.5 - myd / 1000.0))
          ftd' = M.ftd $ conc beam
          ka = 100.0
          bw = (beamW beam)
          as = R.totalSteelArea $ rebars beam
          k0 = (ka * as) / (C.gamc * bw * myd)
          res1 = 0.33 * (ftd' + k0) * bw *  myd * kw / 1000.0
          res2 = 0.66 * ftd' * bw * myd * kw / 1000.0

-- | Trykkbruddkapasitet, NS3473 12.3.2.5
vccd :: Beam 
        -> C.StaticMoment
        -> Double -- ^ [kN]
vccd beam m = 0.45 * z * bw * fcd' / 1000.0 
            where z = calcZ beam m
                  fcd' = M.fcd $ conc beam
                  bw = (beamW beam)

----------------------------------------------------------------------------------------
---------------------------------------- Diverse --------------------------------------- 
----------------------------------------------------------------------------------------

-- | Minimumsarmering lengdearmering
minAs :: Beam -> Double -- ^ [mm2]
minAs beam = 0.35 * (beamW beam) * h * kw * myftk / C.fsk
    where myftk = M.ftk $ conc beam
          h = beamH beam
          kw = 1.5 - (h/1000.0)

-- | Calc distance d form top concrete to centroid rebars area
calcD :: Beam 
         -> Double       -- ^ [mm]
calcD beam = (beamH beam) - d' 
    where d' = R.ddist (rebars beam)

calcZ :: Beam 
         -> C.StaticMoment   -- ^ [kNm] 
         -> Double           -- ^ [mm]
calcZ beam m | m > 0 = (1.0 - (alphad'*f1)) * d'
             | otherwise = 0.9 * d'
    where d' = (calcD beam)
          alphad' = M.alphad $ conc beam 
          mcd' =  mcd beam 
          f1 = m / mcd'

----------------------------------------------------------------------------------------
---------------------------------------- Bøyler ---------------------------------------- 
----------------------------------------------------------------------------------------

-- | Areal av bøyle som krysser skjærsonen.
-- For standard firkantbøyle = 2 stk stenger
asLink :: Link -> Double -- [mm2]
asLink lnk = 2.0*((diam lnk) * 0.5)**2 * pi

-- | Minimumsarmering mm2 bøyler
-- for et lengdesnitt av bjelken, dvs 
-- resultatet fordeles over lengdesnittet 
minAsLinks :: Beam
             -> Double   -- ^ [mm2]
minAsLinks beam = 0.2 * ac * ftk' / C.fsk
    where ac = ((beamW beam) * 1000.0) -- Lengdesnitt
          ftk' = M.ftk $ conc beam

-- | Minimum senteravstand mellom 
-- bjelkens bøyler
minCcLinks :: Beam
              -> Double -- ^ [mm]
minCcLinks beam = minimum [cc,500,0.6*h06] 
    where cc = 1000.0 * (asLink $ links beam) / (minAsLinks beam) 
          h06 = (beamH beam) - (2*R.ddist (rebars beam))
          

-- | Dimensjonerende senteravstand bøyler link
-- gitt skjærkraft v' = v minus 
-- tverrsnittets skjærkraftkapasitet
ccLinks :: Beam 
           -> C.StaticMoment -- [kNm]
           -> C.Shear        -- [kN]
           -> Maybe Double         -- [mm]
ccLinks beam m v | v' > 0 = Just cc
                 | otherwise = Nothing
    where asl = asLink (links beam)
          z' = calcZ beam m
          v' = v - (vcd beam)
          cc = C.fsd * asl * z' / (v' * 1000.0) 
          

