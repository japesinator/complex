module Data.Complex.ZStar

import Control.Algebra
import Data.Complex
import Data.Floats

||| A type representing ℤ ∪ ∞. Intuitively, a point on the Mobius Sphere.
data ZStar = Finite (Complex Float)
           | Infinity

instance Eq ZStar where
  Infinity   == Infinity   = True
  (Finite a) == (Finite b) = a == b
  _          == _          = False

instance Num ZStar where
  (Finite a) + (Finite b) = Finite $ a + b
  _          + _          = Infinity

  (Finite a) - (Finite b) = Finite $ a - b
  Infinity   - Infinity   = Finite 0
  _          - _          = Infinity

  (Finite a) * (Finite b) = Finite $ a * b
  _          * _          = Infinity

  fromInteger x = Finite $ fromInteger x

  abs Infinity   = Infinity
  abs (Finite a) = Finite $ abs a

instance Neg ZStar where
  negate = (*) (Finite $ -1:+0)

(/) : ZStar -> ZStar -> ZStar
Infinity        / Infinity            = Finite 1
Infinity        / _                   = Infinity
_               / Infinity            = Finite 0
_               / (Finite $ 0.0:+0.0) = Infinity
(Finite $ a:+b) / (Finite $ c:+d)     = Finite $
  ((a * c + b * d) / (c * c + d * d)) :+ ((b * c - a * d) / (c * c + d * d))

sqrt : ZStar -> ZStar
sqrt Infinity   = Infinity
sqrt (Finite n) = Finite $ mkPolar (phase n / 2) (sqrt $ magnitude n)
