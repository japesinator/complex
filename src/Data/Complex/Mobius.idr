module Data.Complex.Mobius

import Control.Algebra
import Data.Complex
import Data.Complex.CStar
import Data.Floats

||| A Mobius transformation is a function of the form f(z) = (az + b)/(cz + d).
||| Mobius transformations form a group, and can be composed with `<+>`.
||| It is important that the user ensures ad - bc ≠ 0, and a `VerifiedMT` class
||| is provided for when this needs to be guaranteed.
|||
||| ```idris example
||| MkMT 1 0 0 1
||| ```
|||
data MT : Type where
  MkMT : (a : CStar) -> (b : CStar) -> (c : CStar) -> (d : CStar) -> MT

||| Apply a Mobius transformation to a point on the Mobius sphere to yield a new point
|||
||| ```idris example
||| apply (MkMT 2 0 0 1) (Finite 3) == Finite 6
||| ```
|||
apply : MT -> CStar -> CStar
apply (MkMT a b c d) z = (a * z + b) / (c * z + d)

instance Semigroup MT where
  (MkMT a b c d) <+> (MkMT a' b' c' d') =
    MkMT (a' * a + b' * c) (a' * b + b' * d) (c' * a + d' * c) (c' * b + d' * d)

instance Monoid MT where
  neutral = MkMT 1 0 0 1

instance Group MT where
  inverse (MkMT a b c d) = MkMT d (-b) (-c) a

||| Mobius transformations can be represented as a two by two matrix [a,b;c,d].
||| This function takes the determinant of that matrix.
|||
||| ```idris example
||| determinant (MkMT 1 0 0 1) == 1
||| ```
|||
determinant : MT -> CStar
determinant (MkMT a b c d) = a * d - b * c

||| Mobius transformations must have a non-zero determinant (be invertible).
||| This function checks that that condition holds.
|||
||| ```idris example
||| valid (MkMT 1 0 0 1) == True
||| valid (MkMT 1 0 1 0) == False
||| ```
|||
valid : MT -> Bool
valid = (/=) 0 . determinant

||| A Mobius transformation which has a non-zero determinant.
class VerifiedMT (m : MT) where
  MTValid : valid m = True

||| Mobius transformations can be represented as a two by two matrix [a,b;c,d].
||| This function calculates the trace of that matrix.
|||
||| ```idris example
||| trace (MkMT 1 0 0 1) == 2
||| ```
|||
trace : MT -> CStar
trace (MkMT a _ _ d) = a + d

||| A Mobius transformation is said to be "normalized" when ad - bc = 1. This
||| converts a Mobius transformation to its normalized form.
|||
||| ```idris example
||| normalize (MkMT 2 0 0 2) == MkMT 1 0 0 1
||| ```
|||
normalize : MT -> MT
normalize (MkMT a b c d) = MkMT (a / sd) (b / sd) (c / sd) (d / sd) where
  sd = sqrt $ determinant $ MkMT a b c d

instance Eq MT where
  (==) m n = let (MkMT a b c d) = normalize m
                 (MkMT e f g h) = normalize n in
                 (a,b,c,d) == (e,f,g,h)

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
||| --------------+-----------
|||
||| ```idris example
||| classify (MkMT 1 0 0 0) = Elliptical
||| classify (MkMT 1 0 0 1) = Parabolic
||| ```
|||
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
||| transformation. Note that they can be equal, zero, or Infinity.
|||
||| ```idris example
||| fixpoints (MkMT 1 0 0 1) == (Infinity, Infinity)
||| ```
|||
fixpoints : MT -> (CStar, CStar)
fixpoints (MkMT a b c d) = (x1, x2) where
  p2 : CStar
  x1 : CStar
  x2 : CStar
  p2 = (d - a) / (2 * c)
  x1 = -p2 + sqrt (p2 * p2 + b / c)
  x2 = -(2 * p2 + x1)
