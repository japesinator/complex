module Data.Complex.Spherical

import Data.Complex
import Data.Complex.ZStar
import Data.Floats

||| Spherical coordinates ϕ and θ of a point on the Mobius Sphere
record Spherical : Type where
  MkSpherical : (phi : Float) -> (theta : Float) -> Spherical

||| Convert a point from ℤ ∪ ∞ to a pair of spherical coordinates
toSpherical : ZStar -> Spherical
toSpherical Infinity   = MkSpherical 0 0
toSpherical (Finite z) = MkSpherical (2 * atan (1 / magnitude z)) (phase z)

||| Convert a pair of spherical coordinates to a point from ℤ ∪ ∞.
fromSpherical : Spherical -> ZStar
fromSpherical (MkSpherical p   t) = if p == 0
                                       then Infinity
                                       else Finite $ mkPolar t (1 / tan (p / 2))
