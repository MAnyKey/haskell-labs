{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read, error)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero (Succ _) = LT
natCmp (Succ _) Zero = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero -. n = Zero
n -. Zero = n
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = case compare of
  LT -> Pair Zero n
  EQ -> Pair natOne Zero
  GT -> let rec = natDivMod (n -. m) m in
    Pair (Succ $ fst rec) (snd rec)
  where compare = natCmp n m

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd n Zero = n
gcd n m = gcd m (n `natMod` m)


-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int =
  Positive Nat |
  NegativeMinusOne Nat
  deriving (Show,Read)

natural (Positive n) = n
natural (NegativeMinusOne _) = error "natural: negative Integer"

fromNatural = Positive

intZero   = Positive Zero
intOne    = Positive $ Succ Zero
intNegOne = NegativeMinusOne Zero

-- n -> - n
intNeg :: Int -> Int
intNeg (Positive Zero) = Positive Zero
intNeg (Positive (Succ n)) = NegativeMinusOne n
intNeg (NegativeMinusOne n) = Positive $ Succ n

intAbs (NegativeMinusOne n) = Positive $ Succ n
intAbs n = n

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Positive n) (Positive m) = natCmp n m
intCmp (NegativeMinusOne _) (Positive _) = LT
intCmp (Positive _) (NegativeMinusOne _) = GT
intCmp (NegativeMinusOne n) (NegativeMinusOne m) = natCmp m n

intEq :: Int -> Int -> Bool
intEq n m = case intCmp n m of
  EQ -> True
  otherwise -> False

intLt :: Int -> Int -> Bool
intLt n m = case intCmp n m of
  LT -> True
  otherwise -> False

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Positive n) .+. (Positive m) = Positive $ n +. m
(NegativeMinusOne n) .+. (NegativeMinusOne m) = NegativeMinusOne $ Succ (n +. m)
(Positive n) .+. (NegativeMinusOne m) =
  if' (natLt n (Succ m))
  (NegativeMinusOne $ m -. n)
  (Positive $ n -. (Succ m))
n@(NegativeMinusOne _) .+. m@(Positive _) = m .+. n

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Positive Zero) .*. (NegativeMinusOne _) = Positive Zero
(Positive n) .*. (Positive m) = Positive $ n *. m
(NegativeMinusOne n) .*. (NegativeMinusOne m) = Positive $ (Succ n) *. (Succ m)
(Positive n) .*. (NegativeMinusOne m) = NegativeMinusOne $ (n *. (Succ m)) -. natOne
n@(NegativeMinusOne _) .*. m@(Positive _) = m .*. n

intSign (Positive Zero) = intZero
intSign (Positive (Succ _)) = intOne
intSign (NegativeMinusOne _) = intNegOne

intDiv :: Int -> Int -> Int
intDiv (Positive n) (Positive m) = Positive $ natDiv n m
intDiv n m = intSign n .*. intSign m .*. (intAbs n `intDiv` intAbs m)





-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show, Read)

ratZero = Rat intZero natOne
ratOne = Rat intOne natOne

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat x y) = Rat (intSign x .*. Positive y) (natural . intAbs $ x)

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat xNum xDenom) (Rat yNum yDenom) = 
  if' (natEq xDenom yDenom)
  (intCmp xNum yNum)
  (ratCmp newX newY)
  where newX = Rat (xNum .*. fromNatural yDenom) (xDenom *. yDenom)
        newY = Rat (yNum .*. fromNatural xDenom) (yDenom *. xDenom)

ratEq :: Rat -> Rat -> Bool
ratEq n m = case ratCmp n m of
  EQ -> True
  otherwise -> False

ratLt :: Rat -> Rat -> Bool
ratLt n m = case ratCmp n m of
  LT -> True
  otherwise -> False

ratReduce (Rat num denom) = Rat (num `intDiv` intGcd) (denom `natDiv` natGcd)
  where natGcd = gcd (natural . intAbs $ num) (denom)
        intGcd = fromNatural natGcd

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat xNum xDenom) %+ (Rat yNum yDenom) = ratReduce $ Rat newNumerator newDenominator
  where denomN = fromNatural xDenom
        denomM = fromNatural yDenom
        newNumerator = (xNum .*. fromNatural yDenom) .+. (yNum .*. fromNatural xDenom)
        newDenominator = xDenom *. yDenom

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat xNum xDenom) %* (Rat yNum yDenom) = ratReduce $ Rat (xNum .*. yNum) (xDenom *. yDenom)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)


-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

id x = x

infixr 9 .
f . g = \x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
