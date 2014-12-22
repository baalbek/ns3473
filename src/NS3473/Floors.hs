{-# LANGUAGE NamedFieldPuns, RecordWildCards  #-}
module NS3473.Floors where

import qualified NS3473.Concrete as C

data Floor = Floor {
                h :: Double,
                conc :: C.Concrete 
            } deriving Show

defaultFloor :: Double -> Floor
defaultFloor floorHeight = Floor floorHeight $ C.createConc "35"
   
mcd :: Floor -> Double
mcd f = 0.275 * 1000 * d * d * 16 / 1000000.0
    where d = 160

calcZ :: Floor -> Double -> Double
calcZ f@Floor { h } moment = (1.0 - fact) * d
    where mcd' = mcd f
          fact = 0.165 * moment / mcd'
          d = 160

calcAs :: Floor -> Double -> Double
calcAs f mdim = mdim * 1000000 / (z' * 400) 
    where z' = calcZ f mdim

