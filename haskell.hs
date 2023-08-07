import Data.Monoid


range :: Integral a => a -> a -> [a]
range m n =
  if m == n then [ n ]
  else m : range (m + 1) n


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x
                   then x : filter' f xs
                   else filter' f xs


-- foldl'
-- - fold the list from left-to-right.
-- - tail recursive.
-- *Main> foldl' (-) 0 [1..4]
-- -10
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs


-- foldr'
-- - fold the list from right-to-left.
-- *Main> foldr' (-) 0 [1..4]
-- -2
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)


elem' :: Eq a => a -> [ a ] -> Bool
elem' v [] = False
elem' v (x:xs) = if v == x then True else elem' v xs


zip' :: [a] -> [b] -> [ (a, b) ]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)


gcd' :: Int -> Int -> Int
gcd' m 0 = m
gcd' m n = gcd' n (m `rem` n)


length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs


append' :: [a] -> [a] -> [a]
append' [] ys = ys
append' (x:xs) ys = x : append' xs ys


reverse' :: [a] -> [a]
reverse' xs = foldl' (\a e -> e : a) [] xs


init' [ x ] = []
init' (x:xs) = x : init' xs


last' [ x ] = x
last' (x:xs) = last' xs


palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse' xs


take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs


drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (x:xs) = drop' (n - 1) xs


insertionsort_insert :: Ord a => (a -> a -> Bool) -> a -> [a] -> [a]
insertionsort_insert f v [] = [ v ]
insertionsort_insert f v (x:xs) = if f v x
                                  then v : x : xs
                                  else x : insertionsort_insert f v xs


insertionsort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
insertionsort _ [] = []
insertionsort f (x:xs) = insertionsort_insert f x (insertionsort f xs)


mergesort_merge :: Ord a => (a -> a -> Bool) -> [a] -> [a] -> [a]
mergesort_merge _ xs [] = xs
mergesort_merge _ [] ys = ys
mergesort_merge f (x:xs) (y:ys) = if f x y
                                  then x : mergesort_merge f xs (y : ys)
                                  else y : mergesort_merge f (x : xs) ys


mergesort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
mergesort _ [] = []
mergesort _ [ x ] = [ x ]
mergesort f xs =
  let length = length' xs `div` 2
      left = take' length xs
      right = drop' length xs
  in mergesort_merge f (mergesort f left) (mergesort f right)


quicksort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
quicksort _ [] = []
quicksort f (x:xs) =
  let left = filter' (\e -> f e x) xs
      right = filter' (\e -> not (f e x)) xs
  in quicksort f left ++ [ x ] ++ quicksort f right


sum' :: Num a => [a] -> a
sum' xs = foldl' (+) 0 xs


product' :: Num a => [a] -> a
product' xs = foldl' (*) 1 xs


sieve :: Int -> [Int]
sieve n =
  let sievelist [] = []
      sievelist (x:xs) = x : sievelist (filter (\e -> e `rem` x /= 0) xs)
  in
    sievelist [2..n]


primes :: [ Integer ]
primes =
  let primes (x:xs) = x : primes (filter (\e -> e `rem` x /= 0) xs)
  in primes [2..]


fizzbuzzes :: [ String ]
fizzbuzzes =
  let fizzbuzz n =
        if (n `rem` 3 == 0) && (n `rem` 5 == 0)
        then "fizzbuzz"
        else if n `rem` 3 == 0
        then "fizz"
        else if n `rem` 5 == 0
        then "buzz"
        else show n
  in map' fizzbuzz [1..]


fizzbuzzes2 :: [ String ]
fizzbuzzes2 =
  let fizzes = cycle [ "", "", "fizz" ]
      buzzes = cycle [ "", "", "", "", "buzz" ]
      fizzesbuzzes = zipWith (\a b -> a <> b) fizzes buzzes
      fizzesbuzzesns = zipWith (\a b -> if b == ""
                                        then show a
                                        else b) [1..] fizzesbuzzes
  in fizzesbuzzesns


fibonacci :: Int -> Int
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


fibonaccis :: [Integer]
fibonaccis =
  let fibonaccis m n = m : fibonaccis n (m + n)
  in fibonaccis 0 1


fibonaccis2 :: [Integer]
fibonaccis2 = 0 : 1 : zipWith (+) fibonaccis2 (tail fibonaccis)


pascals =
  let pascal [] = [ 1 ]
      pascal l = zipWith (+) (0 : l) (l ++ [0])
      pascals l = pascal l : pascals (pascal l)
  in pascals []


data ListMap a b = ListMapNil | ListMapData a b (ListMap a b)
                   deriving (Eq, Show)


listmapnew :: ListMap a b
listmapnew = ListMapNil


listmapset :: Eq a => ListMap a b -> a -> b -> ListMap a b
listmapset ListMapNil key value =
  ListMapData key value ListMapNil
listmapset (ListMapData key value next) key' value' =
  if key' == key
  then ListMapData key value' next
  else ListMapData key value (listmapset next key' value')


listmapget :: Eq a => ListMap a b -> a -> Maybe b
listmapget ListMapNil key = Nothing
listmapget (ListMapData key value next) key' =
   if key' == key
   then Just value
   else listmapget next key'


listmapdelete :: Eq a => ListMap a b -> a -> ListMap a b
listmapdelete ListMapNil key = ListMapNil
listmapdelete (ListMapData key value next) key' =
  if key' == key
  then next
  else ListMapData key value (listmapdelete next key')


listmapfrompairs :: Eq a => [(a, b)] -> ListMap a b
listmapfrompairs l =
  foldl (\listmap (key, value) -> listmapset listmap key value) ListMapNil l


listmaptopairs :: ListMap a b -> [(a, b)]
listmaptopairs ListMapNil = []
listmaptopairs (ListMapData key value next) = (key, value) : listmaptopairs next


listmapkeys :: ListMap a b -> [a]
listmapkeys t = map fst (listmaptopairs t)


listmapvalues :: ListMap a b -> [b]
listmapvalues t = map snd (listmaptopairs t)


listmapfoldl :: (c -> b -> c) -> c -> ListMap a b -> c
listmapfoldl _ v ListMapNil = v
listmapfoldl f v (ListMapData key value next) = listmapfoldl f (f v value) next


listmapfoldr :: (b -> c -> c) -> c -> ListMap a b -> c
listmapfoldr _ v ListMapNil = v
listmapfoldr f v (ListMapData key value next) = f value (listmapfoldr f v next)


listmapmap :: (b -> c) -> ListMap a b -> ListMap a c
listmapmap _ ListMapNil = ListMapNil
listmapmap f (ListMapData key value next) =
  ListMapData key (f value) (listmapmap f next)


listmapsize :: ListMap a b -> Int
listmapsize listmap = listmapfoldl (\a _ -> a + 1) 0 listmap


instance Foldable (ListMap a) where
  foldl = listmapfoldl
  foldr = listmapfoldr


instance Functor (ListMap a) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap = listmapmap


data TreeMap a b = TreeMapNil | TreeMapData a b (TreeMap a b) (TreeMap a b)
                   deriving (Eq, Show)


treemapnew :: TreeMap a b
treemapnew = TreeMapNil


treemapset :: Ord a => TreeMap a b -> a -> b -> TreeMap a b
treemapset TreeMapNil key value =
  TreeMapData key value TreeMapNil TreeMapNil
treemapset (TreeMapData key value left right) key' value' =
  case compare key' key of
    LT -> TreeMapData key value (treemapset left key' value') right
    EQ -> TreeMapData key value' left right
    GT -> TreeMapData key value left (treemapset right key' value')


treemapget :: Ord a => TreeMap a b -> a -> Maybe b
treemapget TreeMapNil key = Nothing
treemapget (TreeMapData key value left right) key' =
  case compare key' key of
    LT -> treemapget left key'
    EQ -> Just value
    GT -> treemapget right key'


treemapdelete :: Ord a => TreeMap a b -> a -> TreeMap a b
treemapdelete TreeMapNil key = TreeMapNil
treemapdelete (TreeMapData key value left right) key' =
  case compare key' key of
    LT -> TreeMapData key value (treemapdelete left key') right
    EQ -> case (left, right) of
            (TreeMapNil, right) -> right
            (left, TreeMapNil)  -> left
            (left, right)       ->
              case treemapsmallestkey right right of
                (TreeMapData key'' value'' left'' right'') ->
                  TreeMapData key'' value'' left (treemapdelete right key'')
    GT -> TreeMapData key value left (treemapdelete right key')


treemapsmallestkey :: Ord a => TreeMap a b -> TreeMap a b -> TreeMap a b
treemapsmallestkey TreeMapNil treemapdata = treemapdata
treemapsmallestkey (TreeMapData key value left right)
                   (TreeMapData key' value' left' right') =
  if key <= key'
  then treemapsmallestkey left (TreeMapData key value left right)
  else TreeMapData key' value' left' right'


treemapfrompairs :: Ord a => [(a, b)] -> TreeMap a b
treemapfrompairs l =
  foldl (\treemap (key, value) -> treemapset treemap key value) TreeMapNil l


treemaptopairs :: TreeMap a b -> [(a, b)]
treemaptopairs TreeMapNil = []
treemaptopairs (TreeMapData key value left right) =
  treemaptopairs left ++ [ (key, value) ] ++ treemaptopairs right


treemapkeys :: TreeMap a b -> [a]
treemapkeys t = map fst (treemaptopairs t)


treemapvalues :: TreeMap a b -> [b]
treemapvalues t = map snd (treemaptopairs t)


treemapfoldl :: (c -> b -> c) -> c -> TreeMap a b -> c
treemapfoldl _ v TreeMapNil = v
treemapfoldl f v (TreeMapData key value left right) =
  let aleft = treemapfoldl f v left
      acurrent = f aleft value
      aright = treemapfoldl f acurrent right
  in aright


treemapfoldr :: (b -> c -> c) -> c -> TreeMap a b -> c
treemapfoldr _ v TreeMapNil = v
treemapfoldr f v (TreeMapData key value left right) =
  let aright = treemapfoldr f v right
      acurrent = f value aright
      aleft = treemapfoldr f acurrent left
  in aleft


treemapmap :: (b -> c) -> TreeMap a b -> TreeMap a c
treemapmap _ TreeMapNil = TreeMapNil
treemapmap f (TreeMapData key value left right) =
  TreeMapData key (f value) (treemapmap f left) (treemapmap f right)


treemapsize :: TreeMap a b -> Int
treemapsize treemap = treemapfoldl (\a _ -> a + 1) 0 treemap


instance Foldable (TreeMap a) where
  foldl = treemapfoldl
  foldr = treemapfoldr


instance Functor (TreeMap a) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap = treemapmap


class Map m where
  mapset :: Ord a => m a b -> a -> b -> m a b
  mapget :: Ord a => m a b -> a -> Maybe b
  mapdelete :: Ord a => m a b -> a -> m a b
  mapfrompairs :: Ord a => [(a, b)] -> m a b
  maptopairs :: m a b -> [(a, b)]
  mapkeys :: m a b -> [a]
  mapvalues :: m a b -> [b]
  mapfoldl :: (c -> b -> c) -> c -> m a b -> c
  mapfoldr:: (b -> c -> c) -> c -> m a b -> c
  mapmap :: (b -> c) -> m a b -> m a c
  mapsize :: m a b -> Int


instance Map ListMap where
  mapset = listmapset
  mapget = listmapget
  mapdelete = listmapdelete
  mapfrompairs = listmapfrompairs
  maptopairs = listmaptopairs
  mapkeys = listmapkeys
  mapvalues = listmapvalues
  mapfoldl = listmapfoldl
  mapfoldr = listmapfoldr
  mapmap = listmapmap
  mapsize = listmapsize


instance Map TreeMap where
  mapset = treemapset
  mapget = treemapget
  mapdelete = treemapdelete
  mapfrompairs = treemapfrompairs
  maptopairs = treemaptopairs
  mapkeys = treemapkeys
  mapvalues = treemapvalues
  mapfoldl = treemapfoldl
  mapfoldr= treemapfoldr
  mapmap = treemapmap
  mapsize = treemapsize


data Maybe' a = Nothing' | Just' a
                deriving Show


instance Functor Maybe' where
  fmap f Nothing'  = Nothing'
  fmap f (Just' x) = Just' (f x)


instance Applicative Maybe' where
  pure                = Just'
  Just' f <*> Just' x = Just' (f x)
  _ <*> _             = Nothing'


instance Monad Maybe' where
  Nothing' >>= _  = Nothing'
  Just' x >>= f   = f x


data Probability = Probability String Double deriving (Eq, Show)


heads = Probability "heads" 0.5
tails = Probability "tails" 0.5


combinestrings :: String -> String -> String
combinestrings s1 "" = s1
combinestrings "" s2 = s2
combinestrings s1 s2 = s1 <> "-" <> s2


-- class Semigroup a where
--   (<>) :: a -> a -> a


instance Semigroup Probability where
  (<>) (Probability text1 double1) (Probability text2 double2) =
    Probability (combinestrings text1 text2) (double1 * double2)


luckycombine = heads <> heads <> heads
-- => Probability "heads-heads-heads" 0.125


-- class Semigroup a => Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a


instance Monoid Probability where
  mempty = Probability "" 1


luckymappend = heads `mappend` heads `mappend` heads
-- => Probability "heads-heads-heads" 0.125


luckymconcat = mconcat([ heads, heads, heads ])
-- => Probability "heads-heads-heads" 0.125


luckyfoldr = foldr (<>) mempty [ heads, heads, heads ]
-- => Probability "heads-heads-heads" 0.125
