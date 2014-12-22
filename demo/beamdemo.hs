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

b = B.defaultBeam 200 500 12

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


