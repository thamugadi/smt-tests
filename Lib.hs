{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Lib where

import Data.SBV
import Control.Monad (replicateM)

constrIterM :: (a -> a) -> Symbolic a -> Int -> (a -> SBool) -> Symbolic ()
constrIterM f arg n constr = arg >>= (\a -> constrIter f a n constr)

constrIter :: (a -> a) -> a -> Int -> (a -> SBool) -> Symbolic ()
constrIter f arg n constr = let iterations = take n $ iterate f arg in do
    mapM_ softConstrain $ map constr iterations

symList :: SymVal a => [Int] -> [a] -> Symbolic [SBV a]
symList indexes list = do
    vars <- replicateM (length indexes) $ free "SYM"
    return $ 
      foldl (\acc (idx, val) -> take idx acc ++ val : drop (idx + 1) acc)
        (map literal list) (zip indexes vars)

symMach :: SymVal a => [a] -> Symbolic (SBV a, [SBV a])
symMach mem = do
  var <- free "v"
  return $ (var, map literal mem)
