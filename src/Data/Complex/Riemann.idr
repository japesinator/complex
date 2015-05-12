module Data.Complex.Riemann

import Data.Complex
import Data.Complex.CStar

riemann : (CStar -> CStar) -> (CStar -> CStar) ->
          Integer -> CStar -> CStar -> CStar
riemann f p steps min max = sum $ zw (*) lengths $ drop 1 $ map f path where
  zw : (a -> b -> c) -> List a -> List b -> List c
  zw _ []        _         = []
  zw _ _         []        = []
  zw f (x :: xs) (y :: ys) = f x y :: zw f xs ys
  init' : List a -> List a
  init' (x :: []) = []
  init' (x :: xs) = x :: init' xs
  path : List CStar
  path = map ( p
             . (+) min
             . (*) (max - min)
             . flip (/) (fromInteger steps)
             . fromInteger) [0..steps]
  lengths : List CStar
  lengths = zw (-) (drop 1 path) (init' path)
