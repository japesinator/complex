module Data.Complex.Mobius

import Control.Algebra
import Data.Complex
import Data.Complex.CStar
import Data.Floats

||| A Mobius transformation is a function of the form f(z) = (az + b)/(cz + d).
||| Mobius transformations form a group, and can be composed with `<+>`.
||| It is important that the user ensures ad - bc ≠ 0, and a `VerifiedMT` class
||| is provided for when this needs to be guaranteed.
data MT : Type where
  MkMT : (a : CStar) -> (b : CStar) -> (c : CStar) -> (d : CStar) -> MT

||| Apply a Mobius transformation to a point on the Mobius sphere to yield a new point
apply : MT -> CStar -> CStar
apply (MkMT a b c d) z = (a * z + b) / (c * z + d)

instance Semigroup MT where
  (MkMT a b c d) <+> (MkMT a' b' c' d') =
    MkMT (a' * a + b' * c) (a' * b + b' * d) (c' * a + d' * c) (c' * b + d' * d)

instance Monoid MT where
  neutral = MkMT 1 0 0 (Finite $ 0:+1)

instance Group MT where
  inverse (MkMT a b c d) = MkMT d (-b) (-c) a

||| Mobius transformations can be represented as a two by two matrix [a,b;c,d].
||| This function takes the determinant of that matrix.
determinant : MT -> CStar
determinant (MkMT a b c d) = a * d - b * c

||| Mobius transformations must have a non-zero determinant (be invertible).
||| This function checks that that condition holds.
valid : MT -> Bool
valid = (/=) 0 . determinant

||| A Mobius transformation which has a non-zero determinant.
class VerifiedMT (m : MT) where
  MTValid : valid m = True

||| Mobius transformations can be represented as a two by two matrix [a,b;c,d].
||| This function calculates the trace of that matrix.
trace : MT -> CStar
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
|||  Normalized   |
||| trace squared | Class
||| --------------+-----------
||| 0 ≤ σ < 4     | Elliptical
||| σ = 4         | Parabolic
||| 4 < σ < ∞     | Hyperbolic
||| σ ∈ ℂ \ [0,4] | Loxodromic
classify : MT -> MTClass
classify mt = let m = normalize mt in
                  case trace m of
                       Infinity   =>                               Hyperbolic
                       (Finite t) => if t == conjugate t
                              then ( if realPart (t * t) >  4 then Hyperbolic
                                else if realPart (t * t) == 4 then Parabolic
                                                              else Elliptical
                                   )                          else Loxodromic

||| This function returns the a tuple of the fixed points of a given Mobius
||| transformation. Note that they can be equal, zero, or ∞.
fixpoints : MT -> (CStar, CStar)
fixpoints (MkMT a b c d) = (x1, x2) where
  p2 : CStar
  x1 : CStar
  x2 : CStar
  p2 = (d - a) / (2 * c)
  x1 = -p2 + sqrt (p2 * p2 + b / c)
  x2 = -(2 * p2 + x1)
