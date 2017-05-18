{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}
module NS3473.Rebars where

newtype Rebar = 
    Rebar {
        diam :: Double -- ^ Diameter of rod [mm]
    } deriving Show

data RebarCollection = 
    -- | Rebars as laid out in beams with one row
    SingleRowBeamRebars {
        rebar :: Rebar,
        amount :: Double,   -- ^ Total amount of rebars
        cover :: Double -- ^ Cover [mm] including diameter of links
    }
    -- | Rebars as laid out in beams with multiple rows
    | MultiRowBeamRebars {
        rebar :: Rebar,
        amount :: Double,  -- ^ Total amount of rebars
        rows :: Double,    -- ^ Number of rows the amount is distributed on 
        vdist :: Double,    -- ^ Vertical distance between rows 
        cover :: Double -- ^ Cover [mm] including diameter of links
    } 
    -- | Rebars as laid out in floors and foundations 
    | PlateRebars {
        rebar :: Rebar,
        cc :: Double,   -- ^ Horizontal center distance between rebars [mm]
        cover :: Double -- ^ Cover [mm] including diameter of links
    }
    | SingleWallRebars {
        rebar :: Rebar,  -- ^ Vertical rebars,
        ccVert :: Double,    -- ^ Vertical center distance 
        horizRebar :: Rebar, -- ^ Horizontal rebars,
        ccHoriz :: Double,    -- ^ Horizontal center distance 
        cover :: Double -- ^ Cover [mm] including diameter of links
    }
    -- | The rebar is laid out on both sides with center distance = cc.
    --   I.e. the total number of rebars pr. meter is: 2 * (1000.0 / cc)
    | DoubleWallRebars {
        rebar :: Rebar,  -- ^ Vertical rebars,
        ccVert :: Double,    -- ^ Vertical center distance 
        horizRebar :: Rebar, -- ^ Horizontal rebars,
        ccHoriz :: Double,    -- ^ Horizontal center distance 
        cover :: Double -- ^ Cover [mm] including diameter of links
    }
    | ColumnRebars {
        rebar :: Rebar,
        amount :: Double,   -- ^ Amount of rebars on one column side
        cover :: Double -- ^ Cover [mm] including diameter of links
    } deriving Show

-- | Calculates total steel area for BeamRebars and ColumnRebars, or total pr 1000 mm for PlateRebars
totalSteelArea :: RebarCollection
                  -> Double -- ^ [mm2]
totalSteelArea r@PlateRebars { rebar,cc } = steelArea r 1000.0
totalSteelArea SingleRowBeamRebars { rebar,amount } = amount * (steelAreaRod rebar)
totalSteelArea ColumnRebars { rebar,amount } = amount * (steelAreaRod rebar)
totalSteelArea MultiRowBeamRebars { rebar,amount } = amount * (steelAreaRod rebar)

-- | Area of single rebar
steelAreaRod :: Rebar 
                -> Double -- ^ [mm2]
steelAreaRod Rebar { diam } = (diam * 0.5)**2 * pi

-- | Area of rebars pr areaWidth 
steelArea :: RebarCollection
             -> Double -- ^ Width of area 
             -> Double -- ^ [mm2]
steelArea PlateRebars { rebar,cc } areaWidth = numRebars * (steelAreaRod rebar) 
    where numRebars = areaWidth / cc

-- | Returns height of rebars that occupies d, that is total vertical heigth of rebar area
dsec :: RebarCollection
       -> Double
dsec PlateRebars { rebar } = diam rebar
dsec ColumnRebars { rebar } = diam rebar
dsec MultiRowBeamRebars { rebar,rows,vdist } = 
    ((rows - 1) * vdist) + (rows * (diam rebar))

-- | Returns distance from bottom concrete to centroid rebar collection,
-- i.e. returns h - d
ddist :: RebarCollection
         -> Double -- ^ [mm]
ddist PlateRebars { rebar,cover } = cover + ((diam rebar) * 0.5)
ddist SingleRowBeamRebars { rebar,cover } = cover + ((diam rebar) * 0.5)
ddist ColumnRebars { rebar,cover } = cover + ((diam rebar) * 0.5)
ddist r@MultiRowBeamRebars { cover } = cover + ((dsec r) * 0.5)


