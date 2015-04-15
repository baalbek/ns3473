module NS3473.Buckling where

import qualified NS3473.Common as C
import qualified NS3473.Rebars as R

type RebarFraction = Double -- ^ Andel armering gitt betongtverrsnitt (lest fra kurveskare, n og m)

class Bucklable a where 
    ac       :: a -> Double -- ^ Betongtverrsnittets areal [mm2]
    ic       :: a -> Double -- ^ Betongtverrsnittets annet arealmoment [mm2]
    fcd      :: a -> Double -- ^ Betongens fcd [N/mm2]
    emodulus :: a -> Double -- ^ Betongens emodul [N/mm2]
    hx       :: a -> Double -- ^ Distanse mellom armeringslag [mm]
    hk       :: a -> Double -- ^ Bredde/tykkelse i knekningsretning [mm]
    tx       :: a -> Double -- ^ Betongtverrsnittets tykkelse [mm]
    calcD    :: a -> Double -- ^ Avstand armering / betongtverrsnitt topp [mm]
    lk       :: a -> Double -- ^ Knekklengde [mm]
    wt       :: a -> Double -- ^ Armeringens vektet totale andel (begge sider) av betongtverrsnittet [mm2]

-- | Kurveskarens n faktor [a -> kN] 
nf :: Bucklable a => a -> C.Load -> Double
nf bux p = (p * 1000.0) / (ac' * fcd')
    where ac' = ac bux
          fcd' = fcd bux

-- | Kurveskarens m faktor [a -> kN -> kNm] 
factM :: Bucklable a => a -> C.StaticMoment -> Double
factM bux m = (m * 1000000.0) / (ac' * h * fcd') 
    where h = hk bux
          ac' = ac bux
          fcd' = fcd bux
{-
factM :: Bucklable a => a -> C.Load -> C.StaticMoment -> Double
factM bux n m = (mf * 1000000.0) / (ac' * h * fcd')
    where mf | n > 0.0 = n * 1000.0 * (eTot n m)
             | otherwise = m
          h = hx bux
          ac' = ac bux
          fcd' = fcd bux
-}

-- | Total eksentrisitet [kN -> kNm] 
eTot :: C.Load -> C.StaticMoment -> Double
eTot n m = (e0 n m) + (150.0 / 1000.0)

-- | 1. ordens snittkrefter [kN -> kNm] 
e0 :: C.Load -> C.StaticMoment -> Double
e0 n m | n > 0.0 = m' / n'
       | otherwise = 0.0
    where n' = n * 1000.0
          m' = m * 10**6

-- | Total 1. ordens eksentrisitet
e1 :: Bucklable a => a 
                     -> C.Load         -- ^ [kN]
                     -> C.StaticMoment -- ^ [kNm]
                     -> Double         -- ^ [mm]
e1 bux n m = (e0 n m) + (ns12_2_3 bux)

 -- | Andre-ordens forskyvninger, elastiske forskyvninger, [a -> kN]
ae :: Bucklable a => a -> C.Load -> Double 
ae bux p = (h * h) / (10.0 * r)
    where f = (ac bux) * (fcd bux) * 0.5
          h = lk bux
          pN = p * 1000.0
          inv_r' = (0.0025 + 0.003) / (calcD bux)
          inv_r | pN > f = (f * inv_r') / pN
                | otherwise = inv_r'
          r = 1.0 / inv_r

-- | Kryp, NS 12.2.1
creep :: Bucklable a => a 
                        -> C.Load         -- ^ Normalkraft, n [kN]
                        -> C.StaticMoment -- ^ Moment, m [kNm] 
                        -> Double         -- ^ [mm]
creep bux n m = (e1l*0.8*creepfactor)/(n' - 1 - (0.4*creepfactor))
    where nel = pi**2 * ei / lk2 
          lk2 = (lk bux)**2
          ei = calcEI bux
          creepfactor = 2
          n' = nel/(n*1000) -- n omgjøres til N siden nel kommer ut i N 
          e1l = (e0 n m) + (ns12_2_3 bux)


-- | EI for tverrsnitt
calcEI :: Bucklable a => a 
                        -> Double
calcEI bux = eil -- ^ For rektangulært tverrsnit, Røhne/Vangestad s. 191
    where eil = (emodulus bux) * (ic bux) * (0.6 + 2*(wt bux))

-- | Utilsiktet eksentrisitet (ea), NS 12.2.3
--   og minste tillatte eksentrisitet, NS 12.1.2
ns12_2_3 :: Bucklable a => a -> Double
ns12_2_3 bux = maximum [ex1, ex2, ex3]
    where ex1 = (lk bux) / 300.0
          ex2 = (tx bux) / 30.0 
          ex3 = 20.0
          -- e4 = (hx bux) / 30.0

-- | Nødvendig armeringsmengde git armeringsandel fra kurveskare
calcSteel :: Bucklable a => a 
                            -> RebarFraction 
                            -> Double
calcSteel bux w = w * (ac bux) * (fcd bux) / C.fsd

-- | Knekklengde lk (mm)/ treghetsradius, i
lambda :: Bucklable a => a -> Double
lambda bux = (lk bux) / i
    where i = sqrt ((ic bux) / (ac bux))

-- | Sjekke om kan se bort fra kryp eller 2. ordens eksentrisitet
--   => lambdaf < 10.0, Røhne/Vangestad s. 204
lambdaF :: Bucklable a => a 
                          -> C.Load  -- ^ [kN]
                          -- RebarFraction -> -- ^ armeringsforhold (mywt) (se kurveskare)
                          -> Double
lambdaF bux p = a * (sqrt ( b / (1 + (5*wt'))))
    where a = lambda bux 
          b = nf bux p
          wt' = wt bux

-- | Default armeringsmengdeforhold, as + as' = 1.5%
defwt :: Bucklable a => a -> RebarFraction 
defwt wall = 0.015 * C.fsd / (fcd wall)

{-
-- | Kalkuler vektet (med materialfasthet) andel 
-- armeringtverrsnitt/betongtverrsnitt for en side av tverrsnittet,
-- dvs resultat * 2 = total andel armering 
calcW :: Bucklable a => a 
                        -> Double -- ^ Armering for en side av tverrsnittet [mm2]
                        -> RebarFraction 
calcW bux rebar = (C.fsd * rebar) / ((ac bux) * (fcd bux))
-}
