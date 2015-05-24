module Data.Complex.Spherical

import Control.Algebra
import Data.Complex
import Data.Complex.CStar
import Data.Floats

||| Spherical coordinates ϕ and θ of a point on the Mobius Sphere
record Spherical where
  constructor MkSpherical
  phi   : Float
  theta : Float

instance Eq Spherical where
  (MkSpherical p t) == (MkSpherical p' t') = p == p' && t == t'

||| Convert a point from ℂ ∪ ∞ to a pair of spherical coordinates
toSpherical : CStar -> Spherical
toSpherical Infinity   = MkSpherical 0 0
toSpherical (Finite z) = MkSpherical (2 * atan (1 / magnitude z)) (phase z)

||| Convert a pair of spherical coordinates to a point from ℂ ∪ ∞.
fromSpherical : Spherical -> CStar
fromSpherical (MkSpherical p t) = if p == 0
                                     then Infinity
                                     else Finite $ mkPolar t (1 / tan (p / 2))

instance Num Spherical where
  a + b = toSpherical $ (fromSpherical a) + (fromSpherical b)

  a - b = toSpherical $ (fromSpherical a) - (fromSpherical b)

  a * b = toSpherical $ (fromSpherical a) * (fromSpherical b)

  fromInteger = toSpherical . fromInteger

  abs = toSpherical . abs . fromSpherical

(/) : Spherical -> Spherical -> Spherical
a / b = toSpherical $ (fromSpherical a) / (fromSpherical b)

instance Neg Spherical where
  negate (MkSpherical p t) = MkSpherical p (t + pi)

instance Semigroup Spherical where
  (<+>) = (+)

instance Monoid Spherical where
  neutral = 0

instance Group Spherical where
  inverse = (* -1)

instance AbelianGroup Spherical

instance Ring Spherical where
  (<.>) = (*)

instance RingWithUnity Spherical where
  unity = 1

sqrt : Spherical -> Spherical
sqrt = toSpherical . sqrt . fromSpherical
