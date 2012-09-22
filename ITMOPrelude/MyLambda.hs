module Lambda where

import Prelude hiding (iterate, elem)

type Variable = String

-- Лямбда-терм
data Term = Var Variable
          | Abs Variable Term
          | App Term Term
          deriving (Show)

-- Тип [ a ] == Типу List a
-- значение [] == значению Nil
-- значение [ a ] == значению Cons a Nil
-- конструктор (:) == конструктору Cons

-- Свободные переменные терма
free (Var v) = [v]
free (Abs v t) = filter (/= v) . free $ t -- /= это <<не равно>>
free (App t t') = (free t) ++ (free t')

binded (Var v) = []
binded (Abs v t) = v : binded t
binded (App t t') = binded t ++ binded t'

-- Заменить все свободные вхождения переменной var на what в терме term
subst var what term = case term of
    Var v    -> if v == var then what else term
    Abs v t  -> if v == var then term else Abs v (subst var what t)
    App t t' -> App (subst var what t) (subst var what t')

-- Содержит ли список элемент?
elem a [] = False
elem a (l:ls) = if a == l then True else elem a ls

-- Любопытная функция
iterate f x = (:) x $ iterate f (f x)

-- Генерирует список имён, производных от v, не входящих в fv
newname fv v = head . filter (\x -> not . elem x $ fv) . iterate ('_':) $ v

-- Обычная бета-редукция, хендлящая переименования переменных
betaReduct :: Variable -> Term -> Term -> Term
betaReduct var what term = subst var what $ renameBindings (free what) term
  where renameBindings vars subterm = case subterm of 
          Var _ -> subterm
          App t t' -> App (renameBindings vars t) (renameBindings vars t')
          Abs n t -> Abs nn newt
            where nameUsed = elem n vars
                  nn = if nameUsed then newname (vars ++ free t) n else n
                  newt = if nameUsed then subst n (Var nn) t else t

hasRedexes (Var _) = False
hasRedexes (Abs v t) = hasRedexes t
hasRedexes (App (Abs _ t) t') = True
hasRedexes (App t t') = hasRedexes t || hasRedexes t'

-- Нормализация нормальным порядком терма term
normal' :: Term -> Term
normal' term = if (hasRedexes term) then normal' $ normalReduce term else term 
        
normalReduce term = case term of
  Var _ -> term
  Abs var subterm -> Abs var $ normalReduce subterm
  App (Abs var subterm) term' -> betaReduct var term' subterm
  App term term' -> if hasRedexes term
                    then App (normalReduce term) term'
                    else App term $ normalReduce term'

-- Нормализация аппликативным порядком терма term
applicative' :: Term -> Term
applicative' term = if (hasRedexes term) then applicative' $ applicativeReduce term else term

applicativeReduce term = case term of
  Var _ -> term
  Abs var subterm -> Abs var $ applicativeReduce subterm
  App term term' -> if hasRedexes term' then App term $ applicativeReduce term' else case term of
    Abs v subt -> betaReduct v term' subt
    _ -> App (applicativeReduce term) term'


-- Маркер конца ресурсов
data TooLoong = TooLoong deriving Show

-- (*) Нормализация нормальным порядком терма term за неболее чем n шагов.
-- Результат: Или числа итераций недостаточно, чтобы достичь нормальной
-- формы. Или (число нерастраченных итераций, терм в нормальной форме).
-- 
normal :: Int -> Term -> Either TooLoong (Int, Term)
normal n term 
  | n < 0  = Left TooLoong
  | otherwise = if (hasRedexes term) 
                then normal (n - 1) $ normalReduce term
                else Right (n, term)

-- (*) Аналогичная нормализация аппликативным порядком.
applicative :: Int -> Term -> Either TooLoong (Int, Term)
applicative n term 
  | n < 0  = Left TooLoong
  | otherwise = if (hasRedexes term) 
                then applicative (n - 1) $ applicativeReduce term
                else Right (n, term)

-- (***) Придумайте и реализуйте обобщённую функцию, выражающую некоторое
-- семейство стратегий редуцирования. В том смысле, что номальная, нормальная
-- головная, нормальная слабо-головная и аппликативная стратегии
-- при помощи этой функции будут выражаться некоторым элементарным образом.
-- Аргумент n можно отбросить, а можно оставить.
--
-- strategy = ?
--
-- normal = strategy ?
-- hnf = strategy ?
-- whnf = strategy ?
-- applicative = strategy ?
--
-- Какие ещё стратегии редуцирования вы знаете? Можно ли их выразить
-- при помощи этой стратегии? Если да, то как?
-- Если нет, то можно ли реализовать аналогичную функцию для _всех_
-- возможных стратегий редуцирования, а не только для такого семейства?
-- Если да, то как? Если нет, то почему?

--------------------------------------------------------

-- Область тестирования

loop' = Abs "x" $ App (Var "x") (Var "x")
loop = App loop' loop'

u = Abs "a" $ Abs "b" $ App (Var "a") $ App (Var "b") (Var "_b")
v = Abs "a" $ Abs "b" $ App (App (Var "a") (Var "b")) (Var "_b")
w = Abs "a" $ Abs "b" $ Abs "c" $ Abs "d" $ App (App (Var "a") (Var "b")) (App (Var "c") (Var "d"))

main = test 100
    [ ("no", normal)
    , ("ap", applicative) ]
    [ Var "a"
    , u
    , v
    , loop'
    , u `App` Var "a"
    , v `App` Var "a"
    , u `App` Var "b"
    , v `App` Var "b"
    , u `App` Var "_b"
    , v `App` Var "_b"
    , (u `App` Var "_b") `App` Var "_b"
    , (v `App` Var "_b") `App` Var "_b"
    , w
    , w `App` (Abs "a" (Var "a") `App` (Abs "b" $ Var "b"))
    , (w `App` Abs "a" (Var "b")) `App` loop
    , loop
    ]

-- Если вы не понимаете как это работает, то пока и не надо
pall n term  = mapM_ (\(desc, reduce) -> putStr (desc ++ ": ") >> print (reduce n term))

test :: Show a => Int -> [(String, Int -> Term -> a)] -> [Term] -> IO ()
test n funcs = mapM_ (\term -> print term >> pall n term funcs)
