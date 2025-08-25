module TPHaskell where

-- (a) Tipo de dado para representar expressões
data Expr = Sum Expr Expr
          | Mult Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Lit Int
          | Var String
          deriving Show

-- (b) Tipo de dados para representar o resultado da expressão
data Result = Ok Int 
            | Error String 
            deriving Show

applyOperator :: (Int -> Int -> Result) -> Result -> Result -> Result
applyOperator f (Ok n1) (Ok n2) = f n1 n2
applyOperator _ (Error s) _ = Error s
applyOperator _ _ (Error s) = Error s

wrap :: (Int -> Int -> Int) -> (Int -> Int -> Result)
wrap f x y = Ok (f x y)

safeDiv :: Int -> Int -> Result
safeDiv _ 0 = Error "Divisão por zero"
safeDiv n1 n2 = Ok (n1 `div` n2)

-- Função para avaliar uma expressão
eval :: Expr -> Result
eval (Lit n) = Ok n
eval (Var _) = Error "Variáveis não suportadas nesta fase"
eval (Sum e1 e2) = applyOperator (wrap (+)) (eval e1) (eval e2)
eval (Mult e1 e2) = applyOperator (wrap (*)) (eval e1) (eval e2)
eval (Sub e1 e2) = applyOperator (wrap (-)) (eval e1) (eval e2)
eval (Div e1 e2) = applyOperator safeDiv (eval e1) (eval e2)

-- Função para gerar todas as expressões possíveis a partir de uma lista de números
expressions :: [Int] -> [Expr]
expressions [] = []
expressions [n] = [Lit n]
expressions ns = 
  [ op le re
  | (es, ds) <- splits ns 
  , le <- expressions es 
  , re <- expressions ds
  , op <- [Sum, Mult, Sub, Div]
  ]

-- Função para dividir uma lista em duas partes não vazias
splits :: [a] -> [([a], [a])]
splits [] = []
splits [_] = []
splits (x:xs) = ([x], xs) : [ (x:xs, ds) | (es, ds) <- splits xs ]
