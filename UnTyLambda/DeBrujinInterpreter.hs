module UnTyLambda.DeBrujinInterpreter where

import qualified UnTyLambda.Interpreter as I (Term(..), Variable, free)

import Control.Monad (liftM)
import System.IO.Unsafe (unsafePerformIO)

assoc key [] = Nothing
assoc key (a:alist)
  | key == (fst a) = Just a
  | otherwise = assoc key alist


acons newkey newval alist = (newkey, newval) : alist

data DBTerm = Var Int
            | Abs DBTerm
            | App DBTerm DBTerm
            deriving (Show, Read)

getJust (Just a) = a

type Context = [(I.Variable, Int)]

removenames :: Context -> I.Term -> DBTerm
removenames = removenames' 0
  where removenames' n ctx (I.App lt rt) = App (removenames' n ctx lt) (removenames' n ctx rt)
        removenames' n ctx (I.Abs name subterm) = Abs (removenames' (n + 1) (acons name (- (n + 1)) ctx) subterm)
        removenames' n ctx (I.Var name) = let num = n + (snd . getJust $ assoc name ctx)
                                          in Var num


getDeBrujin :: I.Term -> (DBTerm, Context)
getDeBrujin term = (removenames context term, context)
  where context = getContext 0 $ I.free term
        getContext n (x:xs) = (x,n) : (getContext (n + 1) xs)
        getContext n [] = []
        
-- restorenames :: Context -> DBTerm -> I.Term
-- restorenames ctx term = restorenames' 0
--   where restorenames' n ctx (I.App lt rt) = App (restorenames' n ctx lt) (restorenames' n ctx rt)
--         restorenames' n ctx (I.Abs name subterm) = Abs (restorenames' (n + 1) (acons name (- (n + 1)) ctx) subterm)
--         restorenames' n ctx (I.Var name) = let num = n + (snd . getJust $ assoc name ctx)
--                                           in Var num

shift' d c term = case term of
  (Var num) -> Var $ if num < c
                     then num
                     else num + d
  (Abs subterm) -> Abs $ shift' d (c + 1) subterm
  (App t1 t2) -> App (shift' d c t1) $ shift' d c t2

shift d  = shift' d 0
  

subst what var term = case term of
  (Var num) -> if num == var
              then what
              else term
  (App t1 t2) -> App (subst what var t1) (subst what var t2)
  (Abs subterm) -> Abs $ subst (shift 1 what) (var + 1) subterm

betaReduct :: DBTerm -> DBTerm -> DBTerm
betaReduct what = shift (-1) . subst (shift 1 what) 0

hasRedexes (Var _) = False
hasRedexes (Abs t) = hasRedexes t
hasRedexes (App (Abs t) t') = True
hasRedexes (App t t') = hasRedexes t || hasRedexes t'

-- Нормализация нормальным порядком терма term
normal' :: DBTerm -> DBTerm
normal' term = if (hasRedexes term) then normal' $ normalReduce term else term 
        
normalReduce term = case term of
  Var _ -> term
  Abs subterm -> Abs $ normalReduce subterm
  App (Abs subterm) term' -> betaReduct term' subterm
  App term term' -> if hasRedexes term
                    then App (normalReduce term) term'
                    else App term $ normalReduce term'

-- Нормализация аппликативным порядком терма term
applicative' :: DBTerm -> DBTerm
applicative' term = if (hasRedexes term) then applicative' $ applicativeReduce term else term

applicativeReduce term = case term of
  Var _ -> term
  Abs subterm -> Abs $ applicativeReduce subterm
  App term term' -> if hasRedexes term' 
                    then App term $ applicativeReduce term' 
                    else case term of
                      Abs subt -> betaReduct term' subt
                      _ -> App (applicativeReduce term) term'


-- Маркер конца ресурсов
data TooLoong = TooLoong deriving Show

-- (*) Нормализация нормальным порядком терма term за неболее чем n шагов.
-- Результат: Или числа итераций недостаточно, чтобы достичь нормальной
-- формы. Или (число нерастраченных итераций, терм в нормальной форме).
-- 
normal :: Int -> DBTerm -> Either TooLoong (Int, DBTerm)
normal n term 
  | n < 0  = Left TooLoong
  | otherwise = if (hasRedexes term) 
                then normal (n - 1) $ normalReduce term
                else Right (n, term)

-- (*) Аналогичная нормализация аппликативным порядком.
applicative :: Int -> DBTerm -> Either TooLoong (Int, DBTerm)
applicative n term 
  | n < 0  = Left TooLoong
  | otherwise = if (hasRedexes term) 
                then applicative (n - 1) $ applicativeReduce term
                else Right (n, term)
