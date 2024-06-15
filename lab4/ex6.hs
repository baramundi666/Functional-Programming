class Mappable t where
  fMap :: (a -> b) -> t a -> t b

data Vec3D a = Vec3D {x::a, y::a, z::a} deriving Show

instance Mappable Vec3D where
  fMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)

newtype Pair a = Pair (a,a) deriving Show

instance Mappable Pair where
  fMap f (Pair (x,y)) = Pair (f x, f y)

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)
                 deriving Show

instance Mappable BinTree where
    fMap f EmptyBT = EmptyBT
    fMap f (NodeBT a lt rt) = NodeBT (f a) (fMap f lt) (fMap f rt)

instance Mappable Maybe where
    fMap f Nothing = Nothing
    fMap f (Just a) = Just (f a)

instance Mappable (Either a) where
    fMap _ (Left x) = Left x
    fMap f (Right y) = Right (f y)

instance Mappable ((->) a) where
  fMap f g = f . g

class VectorLike t where
 (|==|) :: Eq a => t a -> t a -> Bool
 (|+|), (|-|) :: (Num a) => t a -> t a -> t a
 (|*|) :: (Num a) => t a -> t a -> a
 (||?), (|-?) :: (Num a, Eq a) => t a -> t a -> Bool -- równoległość i prostopadłość
 vectLength :: Floating a => t a -> a
 unitVectOf :: Floating a => t a -> t a

instance VectorLike Vec3D where
    (|==|) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = a1==a2 && b1==b2 && c1==c2
    (|+|) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = (Vec3D (a1+a2) (b1+b2) (c1+c2))
    (|-|) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = (Vec3D (a1-a2) (b1-b2) (c1-c2))
    (|*|) :: Num a => Vec3D a -> Vec3D a -> a
    (|*|) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = a1*a2+b1*b2+c1*c2
    (||?) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = True
    (|-?) :: (Num a, Eq a) => Vec3D a -> Vec3D a -> Bool
    (|-?) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = True
    vectLength (Vec3D a b c) = sqrt (a**2+b**2+c**2)
    unitVectOf :: Floating a => Vec3D a -> Vec3D a
    unitVectOf (Vec3D a b c) = let new_a = a/len; new_b = b/len;new_c = c/len  in (Vec3D new_a new_b new_c)  where len = vectLength (Vec3D a b c)
