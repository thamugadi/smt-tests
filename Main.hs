{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.SBV
import Data.Word
import Control.Monad
import Lib 

l :: [Word8]
l = [3..250]

f :: Num a => [a] -> [a]
f [] = []
f [x] = [x]
f (x:xs) = [x+5*(head xs)] ++ f xs

g :: (Num a, Mergeable a, EqSymbolic a) => (a, [a]) -> (a, [a])
g (r,m) = ite (r .== 0) (1, m) $
          ite (r ./= 0) (0, m) $
          (r,m)

search :: IO SatResult 
search = do
  sat $ do
    let list = symList [0,1] l
    let cond = \x -> x !! 5 .== 10 .&& x !! 4 .> 1
    constrIterM f list 10 cond
    let cond2 = \(x,y) -> x .== 0
    let mach = symMach [0..100]
    constrIterM (g @SWord8) mach 5 cond2

main :: IO ()
main = do
  res <- search
  print res
