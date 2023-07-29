{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.SBV
import Data.Word
import Control.Monad
import Lib

g :: (Num a, Mergeable a, EqSymbolic a) => (a, [a]) -> (a, [a])
g (r,m) = ite (r .== 0) (1, m) $
          ite (r ./= 0) (0, m) $
          (r,m)

g_inputs :: (Num a, Mergeable a, EqSymbolic a) => [a] -> Int -> [(a, [a])] -> [(a, [a])]
g_inputs [] 0 l = l
g_inputs (i:is) 0 l =
  ite (last (snd (head l)) .== 0) (g_inputs (i:is) 0 (g (i, snd (head l)) : l)) $
  (g_inputs (i:is) 1 (g (i, snd (head l)) : l))
g_inputs (i:is) 1 l =
  ite (last (snd (head l)) .== 0) (g_inputs is 2 (g (i, snd (head l)) : l)) $
  (g_inputs (i:is) 1 (g (i, snd (head l)) : l))
g_inputs (i:is) 2 l = g_inputs (i:is) 0 l

g_inputs_ :: (Num a, Mergeable a, EqSymbolic a) => [a] -> [(a, [a])] -> [(a, [a])]
g_inputs_ l = g_inputs l 0 

search_g :: IO SatResult
search_g = do
  sat $ do
    let list = symList [0..100] (take 5 $ repeat 0)
    list >>=
      (\x -> do
        let result = g_inputs_ @(SBV Word8) x [(0, take 200 $ repeat 0)]
        constrain $ sAny (\(r, m) -> m !! 120 .== 1) result)

main :: IO ()
main = do
  print 1 
