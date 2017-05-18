{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}
import qualified NS3473.Concrete as M
import qualified NS3473.Foundations as F
import qualified NS3473.Rebars as R

conc = M.newConc "35" 

rebar = R.PlateRebars (R.Rebar 10) 140 25

w = F.WallFoundation 150 1000 300 conc rebar

s = F.SquareFoundation 120 1250 240 conc rebar

s2 = F.SquareFoundation 100 1000 200 conc rebar

check :: F.Foundation -> Double -> IO Bool
check f p = do 
    let F.OneFr mcd = F.mcd f
    let areaQ = F.areaQ f p
    let F.OneFr dimM = F.dimMoment f areaQ
    putStrLn ("mcd: " ++ (show mcd))
    putStrLn ("areaQ: " ++ (show areaQ))
    putStrLn ("dimM: " ++ (show dimM))
    return $ mcd > dimM

