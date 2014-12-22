module NS3473.Common where

import qualified NS3473.Utils as U

fsk = 500.0
fsd = 400.0
gamc = 1.4 -- betongens materialkoeffisient

cm = 0.276 -- m for 500 N/mm2 rebar quality

type Steel = Double

type StaticMoment = Double

type Shear = Double

type Load = Double

data MomentType = FieldMoment | SupportMoment

type Degrees = Double

type Radians = Double

type Dconc = Double  -- d 

type Diam = Double  -- d 

toRadians :: Degrees -> Radians
toRadians d = (d/180.0) * pi 

toDegrees :: Radians -> Degrees 
toDegrees r = (r/pi) * 180.0

ro2dec :: Double -> Int -> Double
ro2dec v n = (fromInteger $ round $ v * fact)/fact
    where fact = 10^n



