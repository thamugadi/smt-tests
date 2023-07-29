{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Lib where

import Data.SBV
import Control.Monad (replicateM)

symList :: SymVal a => [Int] -> [a] -> Symbolic [SBV a]
symList indexes list = do
    vars <- replicateM (length indexes) $ free "SYM"
    return $ 
      foldl (\acc (idx, val) -> take idx acc ++ val : drop (idx + 1) acc)
        (map literal list) (zip indexes vars)
