module Data.Complex.Quaternion

import Control.Algebra
import Data.Complex
import Data.Floats
import Data.Vect

||| A type representing an extension of the complex numbers to four dimensions.
||| [a, b, c, d] (Idris) ⇔ a + bi + cj + dk (Mathematics)
Quaternion : Type
Quaternion = Vect 4 Float

||| Extract the real part of a given Quaternion
realPart : Quaternion -> Float
realPart = head

||| Extract the imaginary part of a given Quaternion
imagPart : Quaternion -> Float
imagPart = head . tail

||| Extract the complex part, of form a + bi, of a given Quaternion
complexPart : Quaternion -> Complex Float
complexPart [a,b,_,_] = a :+ b

||| Turn a real number into a Quaternion
fromReal : Float -> Quaternion
fromReal = flip (::) [0,0,0]

||| Turn a complex number into a Quaternion
fromComplex : Complex Float -> Quaternion
fromComplex (a:+b) = [a,b,0,0]

||| Turn a quaternion into its matrix representation
toMatrix : Quaternion -> Vect 2 (Vect 2 (Complex Float))
toMatrix [a,b,c,d] = [[a :+ b, c :+ d], [-c :+ d, a :+ -b]]

||| Get the complement of a given Quaternion
complement : Quaternion -> Quaternion
complement [a,b,c,d] = [a,-b,-c,-d]

||| Find the length of an n-dimensionsal vector
length : Vect n Float -> Float
length = sqrt . with Foldable sum . map (flip pow 2)

||| Normalize an n-dimensionsal vector to length one
normalize : Vect n Float -> Vect n Float
normalize q = map (flip (/) $ length q) q

dotProduct : Quaternion -> Quaternion -> Float
dotProduct p q = Foldable.sum $ zipWith (*) p q

||| The left action of a quaternion on a vector in three dimensions
act : Quaternion -> Vect 3 Float -> Vect 3 Float
act q [x,y,z] = let [a,b,c,d] = normalize q in
  [ x * (pow a 2 + pow b 2 - pow c 2 - pow d 2)
  + y * (2 * (b * c - a * d))
  + z * (2 * (b * d + a * c))
  ,
    x * (2 * (b * c + a * d))
  + y * (pow a 2 - pow b 2 + pow c 2 - pow d 2)
  + z * (2 * (c * d - a * b))
  ,
    x * (2 * (b * d - a * c))
  + y * (2 * (c * d + a * b))
  + z * (pow a 2 - pow b 2 - pow c 2 + pow d 2)
  ]

rotate : Vect 3 Float -> Float -> Quaternion
rotate v t = let [x,y,z] = normalize v
                 s = sin $ 0.5 * t in [cos $ 0.5 * t, x * s, y * s, z * s]

instance Num Quaternion where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) [a1,b1,c1,d1] [a2,b2,c2,d2] = [ a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2
                                    , a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2
                                    , a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2
                                    , a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
                                    ]

  fromInteger = fromReal . fromInteger
  abs         = fromReal . length

instance Neg Quaternion where
  negate = (*) $ fromInteger $ -1

instance Semigroup Quaternion where
  (<+>) = (+)

instance Monoid Quaternion where
  neutral = 0

instance Group Quaternion where
  inverse = (* -1)

instance AbelianGroup Quaternion

instance Ring Quaternion where
  (<.>) = (*)

instance RingWithUnity Quaternion where
  unity = 1

-- Note: Quaternions are not a field proper, as multiplication on quaternions
-- is non-commutative
instance Field Quaternion where
  inverseM q = const $ map ( flip (/)
                           $ with Foldable sum
                           $ map (flip pow 2) q
                           ) $ complement q

slerp : Quaternion -> Quaternion -> Float -> Quaternion
slerp q p t = if (1 - cos phi) < 0.00000001 then q else
  map (flip (/) $ sin phi) (map ((*) $ sin $ phi - t * phi) q)
                         + (map ((*) $ sin $ t * phi)       $ f p)
  where
    dqp : Float
    dqp = dotProduct q p
    f : Quaternion -> Quaternion
    f = if dqp < 0 then negate else id
    phi : Float
    phi = acos $ abs dqp
