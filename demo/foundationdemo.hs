{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}
import qualified NS3473.Concrete as M
import qualified NS3473.Foundations as F
import qualified NS3473.Rebars as R

conc = M.newConc "35" 

rebar = R.FloorRebars (R.Rebar 12) 100 25

w = F.WallFoundation 150 1000 300 conc rebar

s = F.SquareFoundation 200 1500 300 conc rebar

check :: F.Foundation -> Double -> IO Bool
check f m = do 
    let F.OneFr mcd = F.mcd s
    let F.OneFr dimM = F.dimMoment s m
    return $ mcd > dimM

