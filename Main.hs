{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

import Data.SBV
import Data.Word

l :: [Word8]
l = [1..250]

f :: [SBV Word8] -> [SBV Word8]
f [] = []
f [x] = [x]
f (x:xs) = [x+5*(head xs)] ++ f xs

constrIter :: (a -> a) -> a -> Int -> (a -> SBool) -> Symbolic ()
constrIter f arg n constr = let iterations = take n (iterate f arg) in do
    mapM_ softConstrain $ (map constr iterations)

search :: IO ()
search = do
  result <- sat $ do
    a <- free "a"
    b <- free "b"
    let list = [a, b] ++ (map literal (drop 2 l))
    constrIter f list 330 (\x -> x !! 5 .== 10)
  print result

main :: IO ()
main = search
