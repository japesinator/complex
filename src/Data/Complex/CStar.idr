module Data.Complex.CStar

import Control.Algebra
import Data.Complex
import Data.Floats

||| A type representing ℂ ∪ ∞. Intuitively, a point on the Mobius Sphere.
|||
||| ```idris example
||| a : CStar
||| a = Infinity
||| b : CStar
||| b = Finite (3 :+ 4)
||| c : CStar
||| c = Finite 0
||| ```
|||
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

||| CStar division
|||
||| ```idris example
||| (Finite 1) / (Finite 0) == Infinity
||| ```
|||
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

||| Take the conjugate of a given CStar
|||
||| ```idris example
||| conjugate $ Finite (3 :+ 4i) == Finite (3 :+ -4)
||| ```
|||
conjugate : CStar -> CStar
conjugate Infinity   = Infinity
conjugate (Finite a) = Finite $ conjugate a

||| Take the square root of a given CStar
|||
||| ```idris example
||| sqrt $ Finite (0 :+ 2) == Finite (1 :+ -1)`
||| ```
|||
sqrt : CStar -> CStar
sqrt Infinity   = Infinity
sqrt (Finite n) = Finite $ mkPolar (sqrt $ magnitude n) ((phase n) / 2)

||| Find the magnitude of a CStar as a vector
|||
||| ```idris example
||| magnitude $ Finite (3 :+ 4) == 5
||| ```
|||
magnitude : CStar -> Float
magnitude Infinity   = 1.0 / 0
magnitude (Finite z) = magnitude z

||| Take the dot product of two CStars
|||
||| ```idris example
||| dotProduct (Finite $ 3 :+ 4) (Finite $ 5 :+ 6) == Finite 42
||| ```
|||
dotProduct : CStar -> CStar -> CStar
dotProduct Infinity        _               = Infinity
dotProduct _               Infinity        = Infinity
dotProduct (Finite $ a:+b) (Finite $ c:+d) = Finite $ a * b + c * d :+ 0

||| Take the cross product of two CStars
|||
||| ```idris example
||| crossproduct (Finite $ 3 :+ 4) (Finite $ 5 :+ 6) == Finite 39
||| ```
|||
crossProduct : CStar -> CStar -> CStar
crossProduct Infinity     _            = Infinity
crossProduct _            Infinity     = Infinity
crossProduct (Finite $ a) (Finite $ b) =
  (Finite $ 1/2:+0) * (Finite $ conjugate a * b + a * conjugate b)
