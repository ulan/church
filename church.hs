{-# LANGUAGE RankNTypes #-}
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


-- Church encoding for pairs

newtype CP a b = CP (forall c. (a -> b -> c) -> c)

instance (Show a, Show b) => Show (CP a b) where
    show (CP x) = show $ x (,)

pair :: a -> b -> CP a b
pair a b = CP $ \f -> f a b

first :: CP a b -> a
first (CP f) = f $ \a b -> a 

second :: CP a b -> b
second (CP f) = f $ \a b -> b 

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

pre :: CN -> CN
pre (CN n) = second (n next (pair zero zero))
  where next p = pair (suc (first p)) (first p)

sub :: CN -> CN -> CN
sub n (CN m) = m pre n

newtype CL a = CL (forall c . (a -> (CL a) -> c) -> c -> c) 

instance Show a => Show (CL a) where
    show x = show (list x)

list (CL x) = x (\a c -> a : list c) [] 

nil = CL $ \f c -> c
cons x xs = CL $ \f c -> f x xs
hd (CL l) = l (\x xs -> x) (hd (CL l))
tl (CL l) = l (\x xs -> xs) (CL l)
mton a b = g a b
  where g a b | a <= b = cons a (g (a + 1) b)
              | otherwise = nil

fold f n (CL k) = k g n
  where g x (CL k) = f x (k g n)

-- main = do print $ fold (+) 0 (mton 0 100000000)