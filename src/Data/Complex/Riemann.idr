module Data.Complex.Riemann

import Data.Complex
import Data.Complex.CStar

||| Approximate an integral using a Riemann sum along a given path
||| @ f   The function to integrate
||| @ p   The path function (`id` for a straight line)
||| @ min `p` inverse of the lower bound
||| @ max `p` inverse of the bound
|||
||| (if p = x * x, min = 2, max = 4, the integral approximated will be from 4 to
||| 16)
riemann : (f : CStar -> CStar) -> (p : CStar -> CStar) ->
          (steps : Integer) -> (min : CStar) -> (max : CStar) -> CStar
riemann f p steps min max = flip (/) 2
                          $ sum
                          $ zw (*) lengths
                          $ zw (+) (drop 1 vals) (init' vals) where
  zw : (a -> b -> c) -> List a -> List b -> List c
  zw _ []        _         = []
  zw _ _         []        = []
  zw f (x :: xs) (y :: ys) = f x y :: zw f xs ys
  init' : List a -> List a
  init' []        = []
  init' (_ :: []) = []
  init' (x :: xs) = x :: init' xs
  path : List CStar
  path = map ( p
             . (+) min
             . (*) (max - min)
             . flip (/) (fromInteger steps)
             . fromInteger) [0..steps]
  lengths : List CStar
  lengths = zw (-) (drop 1 path) (init' path)
  vals : List CStar
  vals = map f path

||| Approximate an integral using a Riemann sum on a domain assumed to be
||| analytic. Uses a straight line from `min` to `max`
||| @ f   The function to integrate
||| @ min The lower bound
||| @ max The upper bound
analRiemann : (f : CStar -> CStar) -> (steps : Integer) ->
              (min : CStar) -> (max : CStar) -> CStar
analRiemann f = riemann f id
