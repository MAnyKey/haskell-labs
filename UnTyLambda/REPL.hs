{-# LANGUAGE ScopedTypeVariables #-}
-- REPL for untyped lambda calculus
module UnTyLambda.REPL where

import Prelude hiding (catch)
import Monstupar
import UnTyLambda.Interpreter
import Data.List

import Control.Exception

-- Парсим строку в терм
parseLambda :: Monstupar Char Term
parseLambda = do
  atom <- parseAtom
  spaces
  other <- parseLambda'
  return $ makeApp atom other
  
makeApp = foldl' App

parseLambda' = (do
  atom <- parseAtom
  spaces
  atoms <- parseLambda'
  return $ atom:atoms) <|> (ok >> return [])

parseAtom = parseAbs <|> parseBraces <|> parseName

parseAbs = do
  char '\\'
  spaces
  name <- parseVarName
  spaces
  char '.'
  spaces
  expr <- parseLambda
  return $ Lam name expr

parseBraces = do
  char '('
  spaces
  expr <- parseLambda
  spaces
  char ')'
  return expr

parseName = do
  name <- parseVarName
  return $ Var name

parseVarName = do
  c <- letter
  cs <- many (letter <|> digit)
  return $ c:cs

letter = oneOf $ ['a'..'z'] ++ ['A'..'Z']
digit = oneOf ['0'..'9']
space = oneOf [' ', '\n', '\r']
spaces = many space
spaces1 = many1 space

--------------------------------------------------------------------------------
-- Заметье, что грамматика лямбда-выражений леворекурсивна.
-- Перед тем как бросаться кодить, сначала уберите леворекурсивность на бумаге,
-- а потом напишите получившуюся грамматику в EBNF вот сюда:
--
-- прямо сюда, да
-- Lambda = Atom Lambda'
-- Lambda' = Atom Lambda'
-- Atom = "\" Var "." Lambda
--      | "(" Lambda ")"
--      | Var
-- Var =  letter (letter | digit)*
--------------------------------------------------------------------------------

-- Красиво печатаем терм (можно с лишними скобками, можно без)
prettyPrint :: Term -> String
prettyPrint (Var x) = x
prettyPrint (App f t) = prettyPrint f ++ " " ++ prettyPrint t
prettyPrint (Lam v t) = "(\\" ++ v ++ " . " ++ prettyPrint t ++ ")"

-- Собственно сам REPL. Первый аргумент — максимальное число итераций при
-- попытке нормализации стратегией из второго аргумента.
replLoop :: Integer -> (Integer -> Term -> Term) -> IO ()
replLoop patience strategy = do
  putStr "> "
  line <- getLine
  case runParser parseLambda line of
    Left _ -> putStrLn "Parse error" 
    Right (_, term) -> catch (
      let newterm = eval term 
      in newterm `seq` putStrLn $ prettyPrint newterm) (
      (\(e :: SomeException) -> (print e)))
  replLoop patience strategy
    where eval term = strategy patience term

-- Диалог с (replLoop 100 no) должен выглядеть так:
-- > \x . (\y . y) x x
-- \x . x x