module Data.Complex.CStar

import Control.Algebra
import Data.Complex
import Data.Floats

||| A type representing ℂ ∪ ∞. Intuitively, a point on the Mobius Sphere.
data CStar = Finite (Complex Float)
           | Infinity

instance Eq CStar where
  Infinity   == Infinity   = True
  (Finite a) == (Finite b) = a == b
  _          == _          = False

instance Num CStar where
  (Finite a) + (Finite b) = Finite $ a + b
  _          + _          = Infinity

  (Finite a) - (Finite b) = Finite $ a - b
  Infinity   - Infinity   = 0
  _          - _          = Infinity

  (Finite a) * (Finite b) = Finite $ a * b
  _          * _          = Infinity

  fromInteger x = Finite $ fromInteger x

  abs Infinity   = Infinity
  abs (Finite a) = Finite $ abs a

(/) : CStar -> CStar -> CStar
Infinity        / Infinity            = 1
Infinity        / _                   = Infinity
_               / Infinity            = 0
(Finite $ a:+b) / (Finite $ c:+d)     = if c == 0 && d == 0
                                           then Infinity
                                           else Finite $
  ((a * c + b * d) / (c * c + d * d)) :+ ((b * c - a * d) / (c * c + d * d))

instance Neg CStar where
  negate = (*) (Finite $ -1:+0)

instance Semigroup CStar where
  (<+>) = (+)

instance Monoid CStar where
  neutral = 0

instance Group CStar where
  inverse = (* -1)

instance AbelianGroup CStar

instance Ring CStar where
  (<.>) = (*)

instance RingWithUnity CStar where
  unity = 1

instance Field CStar where
  inverseM x = const $ 1 / x

instance Show CStar where
  show Infinity   = "Infinity"
  show (Finite z) = "Finite " ++ show z

conjugate : CStar -> CStar
conjugate Infinity   = Infinity
conjugate (Finite a) = Finite $ conjugate a

sqrt : CStar -> CStar
sqrt Infinity   = Infinity
sqrt (Finite n) = Finite $ mkPolar (phase n / 2) (sqrt $ magnitude n)

magnitude : CStar -> Float
magnitude Infinity   = 1.0 / 0
magnitude (Finite z) = magnitude z

dotProduct : CStar -> CStar -> CStar
dotProduct Infinity        _               = Infinity
dotProduct _               Infinity        = Infinity
dotProduct (Finite $ a:+b) (Finite $ c:+d) = Finite $ a * b + c * d :+ 0

crossProduct : CStar -> CStar -> CStar
crossProduct Infinity     _            = Infinity
crossProduct _            Infinity     = Infinity
crossProduct (Finite $ a) (Finite $ b) =
  (Finite $ 1/2:+0) * (Finite $ conjugate a * b + a * conjugate b)
