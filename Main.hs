{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

import Data.SBV
import Data.Word
import Control.Monad

l :: [Word8]
l = [3..250]

f :: Num a => [a] -> [a]
f [] = []
f [x] = [x]
f (x:xs) = [x+5*(head xs)] ++ f xs

constrIterM :: (a -> a) -> Symbolic a -> Int -> (a -> SBool) -> Symbolic ()
constrIterM f arg n constr = arg >>= (\a -> constrIter f a n constr)

constrIter :: (a -> a) -> a -> Int -> (a -> SBool) -> Symbolic ()
constrIter f arg n constr = let iterations = take n (iterate f arg) in do
    mapM_ softConstrain $ (map constr iterations)

symList :: SymVal a => [Int] -> [a] -> Symbolic [SBV a]
symList indexes list = do
    vars <- replicateM (length indexes) (free "SYL")
    return $ 
      foldl
        (\acc (idx, val)
          -> take idx acc ++ val : drop (idx + 1) acc) (map literal list) (zip indexes vars)

search :: IO ()
search = do
  result <- sat $ do
    let list = symList [0,1] l
    let cond = \x -> x !! 5 .== 10 .&& x !! 4 .> 1
    constrIterM f list 300 cond 
  print result

main :: IO ()
main = search
