module NS3473.Cracking where

-- svinn : shrinkage
-- riss  : cracking 
-- creep : kryp

class Crackable a where 
    kt              :: a -> Double
{-
    k2              :: a -> Double    -- ^ Coeffisient for distribution of strains
    es              :: a -> Double    -- ^ Main strain in reinforcement
    e1              :: a -> Double    -- ^ Max strain in concrete 
    e2              :: a -> Double    -- ^ Min strain in concrete
    creepf          :: a -> Double    -- ^ Creep factor, normal: 2.5
    eck             :: a -> Double    -- ^ E-modulus, normal
    ec8             :: a -> Double    -- ^ E-modulus long term
    ec8 x = (eck x) / (1 + (creepf x)) 
    ae              :: a -> Double    -- ^ Efficient tensile area of concrete 
-}
