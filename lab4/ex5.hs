newtype MyInt = MkMyInt Int

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
  (<=) :: MyInt -> MyInt -> Bool
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

instance Eq a => Eq (BinTree a) where
    (==) :: Eq a => BinTree a -> BinTree a -> Bool
    (==) (NodeBT val1 lt1 rt1) (NodeBT val2 lt2 rt2) = val1==val2 && lt1==lt2 && rt1==rt2
