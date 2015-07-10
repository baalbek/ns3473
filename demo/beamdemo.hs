{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}

import Control.Monad.Writer (Writer,runWriter,tell)

import Data.Monoid ((<>))

import Text.Printf (printf)

import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Beams as B
import NS3473.DiffList (DiffList,toDiffList,fromDiffList)

type StringDL = DiffList String 

conc = M.newConc "35" 

rebar = R.SingleRowBeamRebars (R.Rebar 12) 4 25



links = B.Link 8

-- b = B.RectBeam 200 500 conc rebar (B.Link 8)


mcdCheck :: B.Beam  
            -> Double  -- ^ Moment 
            -> Writer StringDL Bool
mcdCheck beam m = do 
    let mcd = B.mcd beam
    tell $ toDiffList [(printf "[%.2f kNm] Mcd: %.2f kNm" m mcd)] 
    if mcd > m then
        return True
    else 
        return False

ccLinksCheck :: B.Beam 
                -> Double  -- ^ Moment 
                -> Double  -- ^ Shear
                -> Writer StringDL Bool
ccLinksCheck beam m v = do
    -- tell $ toDiffList 
    return False

c = fromDiffList ((toDiffList [1,2,3,4]) <> (toDiffList [1,2,3]))

b = B.defaultBeam 350 350 16 4

rebar2 = R.MultiRowBeamRebars (R.Rebar 20) 9 2 25 (25 + 8)
b2 = B.RectBeam 350 500 conc rebar2 links

rebar3 = R.SingleRowBeamRebars (R.Rebar 16) 12 (25 + 8)
b3 = B.RectBeam 750 350 conc rebar3 links

rebar4 = R.SingleRowBeamRebars (R.Rebar 16) 10 (25 + 8)

dc :: Double -> B.DeflectionContext  
dc xi = B.DeflectionContext xi 6200 

b4 :: Double -> Double -> B.Beam
b4 b h = B.RectBeam b h conc rebar4 links

dc5 :: Double -> Double -> B.DeflectionContext  
dc5 xi w = B.DeflectionContext xi w

rebar5 n = R.SingleRowBeamRebars (R.Rebar 8) n (25 + 8)

b5 :: Double -> Double -> B.Beam
b5 h n = B.RectBeam 1000 h conc (rebar5 n) links

rebar6 n = R.SingleRowBeamRebars (R.Rebar 16) n (25 + 8)
b6 :: Double -> Double -> Double -> B.Beam
b6 b h n = B.RectBeam b h conc (rebar6 n) links

xi = B.xiFact M.ee
xi2 = B.xiFact M.eeLt

displayResult :: (Bool,StringDL) -> IO ()
displayResult r = do
    -- putStrLn $ show $ fst r
    mapM_ (putStrLn . ("\t"++)) (fromDiffList $ snd r)
    return ()

check :: B.Beam -> Double -> Double -> IO ()
check beam m v = do
    {-
    let (result,lst) = runWriter (mcdCheck beam)
    if result == True then
        mapM_ putStrLn (fromDiffList $ lst)
    else
        return ()
    -}
    let passedChecks what x = (fst x) == what
    let results = [runWriter (mcdCheck beam m), runWriter (ccLinksCheck beam m v)]
    putStrLn "These went well:"
    mapM_  displayResult $ filter (passedChecks True) results
    putStrLn "And these fucked up:"
    mapM_  displayResult $ filter (passedChecks False) results
    return () 


