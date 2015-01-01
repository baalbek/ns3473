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
        cover :: Double     -- ^ Overdekning [mm]
    }
    -- | Rebars as laid out in beams with multiple rows
    | MultiRowBeamRebars {
        rebar :: Rebar,
        amount :: Double,  -- ^ Total amount of rebars
        rows :: Double,    -- ^ Number of rows the amount is distributed on 
        vdist :: Double,    -- ^ Vertical distance between rows 
        cover :: Double -- ^ Overdekning [mm]
    } 
    -- | Rebars as laid out in floors and foundations 
    | FloorRebars {
        rebar :: Rebar,
        cc :: Double,   -- ^ Vertical center distance between rebars [mm]
        cover :: Double -- ^ Overdekning [mm]
    }
    | ColumnRebars {
    } deriving Show

-- | Calculates total steel area for BeamRebars and ColumnRebars, or total pr 1000 mm for FloorRebars
totalSteelArea :: RebarCollection
                  -> Double -- ^ [mm2]
totalSteelArea r@FloorRebars { rebar,cc } = steelArea r 1000.0
totalSteelArea SingleRowBeamRebars { rebar,amount } = amount * (steelAreaRod rebar)
totalSteelArea MultiRowBeamRebars { rebar,amount } = amount * (steelAreaRod rebar)

-- | Area of single rebar
steelAreaRod :: Rebar 
                -> Double -- ^ [mm2]
steelAreaRod Rebar { diam } = (diam * 0.5)**2 * pi

-- | Area of rebars pr areaWidth 
steelArea :: RebarCollection
             -> Double -- ^ Width of area 
             -> Double -- ^ [mm2]
steelArea FloorRebars { rebar,cc } areaWidth = numRebars * (steelAreaRod rebar) 
    where numRebars = areaWidth / cc

-- | Returns height of rebars that occupies d, that is total vertical heigth of rebar area
dsec :: RebarCollection
       -> Double
dsec FloorRebars { rebar } = diam rebar
dsec MultiRowBeamRebars { rebar,rows,vdist } = 
    ((rows - 1) * vdist) + (rows * (diam rebar))

-- | Returns distance from bottom concrete to centroid rebar collection,
-- i.e. returns h - d
ddist :: RebarCollection
         -> Double -- ^ [mm]
ddist FloorRebars { rebar,cover } = cover + ((diam rebar) * 0.5)
ddist SingleRowBeamRebars { rebar,cover } = cover + ((diam rebar) * 0.5)
ddist r@MultiRowBeamRebars { cover } = cover + ((dsec r) * 0.5)


