module NS3473.Concrete where

data Concrete  = Concrete {
                            fck :: Double,
                            ftk :: Double,
                            ftn :: Double,
                            ftd :: Double,
                            fcn :: Double,
                            fcd :: Double,
                            ee  :: Double,    -- ^ Emodulus [N/mm2]
                            eeLt :: Double,   -- ^ Long term emodulus [N/mm2]
                            mr  :: Double,    -- ^ Used in calculating ConcreteProfile::mcd = mr * b * d * d * fcd
                            alphad :: Double  -- ^ Used in calculating ConcreteProfile::z
                            } deriving Show

type Grade = String

calcEe :: Double      -- ^ fcck 
          -> Double
calcEe myFcck = 9500.0 * myFcck ** 0.3 

calcEeLt :: Double -> Double
calcEeLt myFck = (calcEe myFck) / (1 + 2.5)

newConc :: String -> Concrete
newConc "25" = Concrete { fck = 25,
                             ftk = 2.10,
                             ftn = 1.40,
                             ftd = 1.00,
                             fcn = 16.8,
                             fcd = 12.0, 
                             ee  = calcEe 20,
                             eeLt = calcEeLt 20,
                             mr  = 0.275,         
                             alphad = 0.165 }
newConc "35" = Concrete { fck = 35,
                             ftk = 2.55,
                             ftn = 1.70,
                             ftd = 1.21,
                             fcn = 22.4,
                             fcd = 16.0, 
                             ee  = calcEe 28,
                             eeLt = calcEeLt 28,
                             mr  = 0.275,
                             alphad = 0.165 }
newConc "45" = Concrete { fck = 45,
                             ftk = 2.95,
                             ftn = 2.00,
                             ftd = 1.43,
                             fcn = 28.0,
                             fcd = 20.0, 
                             ee  = calcEe 36,
                             eeLt = calcEeLt 36,
                             mr  = 0.275,
                             alphad = 0.165 }

mcd :: Concrete   
       -> Double   -- ^ Width [mm]
       -> Double   -- ^ d [mm] 
       -> Double   -- ^ [kNm]
mcd conc w d = mr' * w * d * d * fcd' / 1000000.0
    where mr' = mr conc 
          fcd' = fcd conc

{-
class ConcreteProfile a where 
    width     :: a -> Double
    mcd       :: a -> Double 
    concGrade :: a -> Concrete
    dd        :: a -> Double -- ^ Høyde topp profil til centroid armering, d [mm]
    z         :: a 
                 -> Double  -- ^ Dimensjonerende moment [kNm]
                 -> Double  -- ^ [mm]
    z x m = (1 - (alphad' * m')) * (dd x)
        where m' = m / (mcd x) 
              alphad' = alphad $ concGrade x

-}
