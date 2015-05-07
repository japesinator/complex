module Data.Complex.Mobius

import Control.Algebra
import Data.Complex

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

||| A Mobius transformation is a function of the form f(z) = (az + b)/(cz + d).
||| Mobius transformations form a group, and can be composed with `<+>`.
||| It is important that the user ensures ad - bc ≠ 0, and a `VerifiedMT` class
||| is provided for when this needs to be guaranteed.
data MT : Type where
  MkMT : (a : ZStar) -> (b : ZStar) -> (c : ZStar) -> (d : ZStar) -> MT

||| Apply a Mobius transformation to a point on the Mobius sphere to yield a new point
apply : MT -> ZStar -> ZStar
apply (MkMT a b c d) z = (a * z + b) / (c * z + d)

instance Semigroup MT where
  (MkMT a b c d) <+> (MkMT a' b' c' d') =
    MkMT (a' * a + b' * c) (a' * b + b' * d) (c' * a + d' * c) (c' * b + d' * d)

instance Monoid MT where
  neutral = MkMT (Finite 1) (Finite 0) (Finite 0) (Finite $ 0:+1)

instance Group MT where
  inverse (MkMT a b c d) = MkMT d (-b) (-c) a

||| Mobius transformations can be represented as a two by two matrix [a,b;c,d].
||| This function takes the determinant of that matrix.
determinant : MT -> ZStar
determinant (MkMT a b c d) = a * d - b * c

||| Mobius transformations must have a non-zero determinant (be invertible).
||| This function checks that that condition holds.
valid : MT -> Bool
valid = (/=) (Finite 0) . determinant

||| A Mobius transformation which has a non-zero determinant.
class VerifiedMT (m : MT) where
  MTValid : valid m = True

||| Mobius transformations can be represented as a two by two matrix [a,b;c,d].
||| This function calculates the trace of that matrix.
trace : MT -> ZStar
trace (MkMT a _ _ d) = a + d

||| A Mobius transformation is said to be "normalized" when ad - bc = 1. This
||| converts a Mobius transformation to its normalized form.
normalize : MT -> MT
normalize m@(MkMT a b c d) = MkMT (a/determinant m) (b/determinant m)
                                  (c/determinant m) (d/determinant m)

instance Eq MT where
  a == b = (\(MkMT a b c d),(MkMT a' b' c' d') =>
    (a,b,c,d) == (a',b',c',d')) (normalize a) (normalize b)

||| Mobius transformations can be either Elliptical, Parabolic, Hyperbolic, or
||| Loxodromic. This type represents the class of a Mobius transformation.
data MTClass = Elliptical
             | Parabolic
             | Hyperbolic
             | Loxodromic

||| This function takes a Mobius transformation and returns its class.
||| Intuitively:
|||
||| --------------+-----------
||| Trace squared | Class
||| --------------+-----------
||| 0 ≤ σ < 4     | Elliptical
||| σ = 4         | Parabolic
||| 4 < σ < ∞     | Hyperbolic
||| σ ∈ ℂ \ [0,4] | Loxodromic
classify : MT -> MTClass
classify m = case trace m of
                  Infinity   =>                               Hyperbolic
                  (Finite t) => if t == conjugate t
                         then ( if realPart (t * t) >  4 then Hyperbolic
                           else if realPart (t * t) == 4 then Parabolic
                                                         else Elliptical
                              )                          else Loxodromic
