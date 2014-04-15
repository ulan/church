module Church where

-- Church encoding for booleans.

newtype CB = CB (forall a . a -> a -> a)

instance Show CB where
    show x = show (bool x)

instance Eq CB where
    (==) a b = bool (neg $ xor a b)

bool :: CB -> Bool
bool (CB a) = a True False

true :: CB
true  = CB $ \t f -> t

false :: CB
false = CB $ \t f -> f

neg :: CB -> CB
neg (CB a) = CB $ flip a

(.&.) :: CB -> CB -> CB
(.&.) (CB a) b = a b false 


(.|.) :: CB -> CB -> CB
(.|.) (CB a) b = a true b 

xor :: CB -> CB -> CB
xor (CB a) (CB b) = CB $ \t f -> a (b f t) (b t f)


-- Church encoding for natural numbers.

newtype CN = CN (forall a . (a -> a) -> a -> a)

instance Show CN where
    show x = show (num x)

instance Eq CN where
    (==) a b = bool $ (iszero $ sub a b) .&. (iszero $ sub b a)

num :: CN -> Integer
num (CN n) = n (1 +) 0
 
zero :: CN
zero = CN $ \f x -> x

iszero :: CN -> CB
iszero (CN n) = n (\x -> false) true

suc :: CN -> CN
suc (CN n) = CN $ \f x -> n f (f x)

add :: CN -> CN -> CN
add (CN n) (CN m) = CN $ \f x -> (n f) (m f x)

mul :: CN -> CN -> CN
mul (CN n) (CN m) = CN $ \f x -> (n (m f)) x

pow :: CN -> CN -> CN
pow (CN n) (CN m) = CN $ \f x -> m n f x 

cnum :: Integer -> CN
cnum 0 = zero
cnum n = suc $ cnum (n - 1)

pair :: a -> a -> CB -> a
pair n m (CB b) = b n m

first :: (CB -> a) -> a
first p = p true 

second :: (CB -> a) -> a
second p = p false

pre :: CN -> CN
pre (CN n) = second (n next (pair zero zero))
  where next p = pair (suc (first p)) (first p)

sub :: CN -> CN -> CN
sub n (CN m) = m pre n
