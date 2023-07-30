{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.SBV
import Data.Word
import Control.Monad
import Lib

class ExtendedEq a b where
  ifThenElse :: b -> a -> a -> a
  notEqual :: a -> a -> b

instance Eq a => ExtendedEq a Bool where
  ifThenElse cond a b = if cond then a else b
  notEqual a b = a /= b

instance (EqSymbolic a, Mergeable a) => ExtendedEq a SBool where
  ifThenElse = ite
  notEqual a b = a ./= b

--h :: (ExtendedEq a b, Num a) => (a, [a]) -> (a, [a])
--h (r,m) = ifThenElse (r `notEqual` 0) (1, m) $
--          ifThenElse (r `notEqual` 0) (0, m) $
--          (r,m)

g :: (Num a, Mergeable a, EqSymbolic a) => (a, [a]) -> (a, [a])
g (r,m) = ite (r .== 0) (1, m) $
          ite (r ./= 0) (0, m) $
          (r,m)

g_inputs :: (Num a, Mergeable a, EqSymbolic a) => [a] -> Int -> (a, [a]) -> (a, [a])
g_inputs [] 0 (c, m) = (c, m)
g_inputs (i:is) 0 (c,m) =
  ite (m !! 0x4 .== 0) (g_inputs (i:is) 0 (g (c, m))) $ (g_inputs (i:is) 1 (g (i, m)))
g_inputs (i:is) 1 (c,m) =
  ite (m !! 0x4 .== 0) (g_inputs is 2 (g (i, m))) $ (g_inputs (i:is) 1 (g (i, m)))
g_inputs (i:is) 2 (c,m) = g_inputs (i:is) 0 (c,m)

g_inputs_ :: (Num a, Mergeable a, EqSymbolic a) => [a] -> (a, [a]) -> (a, [a])
g_inputs_ = (\x y -> g_inputs x 0 y)

ar :: (Num a, Mergeable a, EqSymbolic a) => [a] -> (a, [a]) -> (a, [a])
ar [] (c, m) = ite (c ./= 4) (c, (c+1):m) (c, c:m)
ar (i:is) (c, m) = ar is (i, m)

search_g :: IO SatResult
search_g = do
  sat $ do
    let list = symList [0..10] (take 10 $ repeat 0)
    list >>=
      (\x -> 
        constrain (((snd $ (ar @(SBV Word8)) x (0, take 30 $ repeat 0)) !! 0) .== 12))

main :: IO ()
main = do
  search_g >>= print 
