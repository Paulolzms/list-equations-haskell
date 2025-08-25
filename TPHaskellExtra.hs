module TPHaskellExtra where

-- Tipo de dado para representar expressões com Int e Bool
data Expr2 = Sum Expr2 Expr2
           | Mult Expr2 Expr2
           | Sub Expr2 Expr2
           | Div Expr2 Expr2
           | Less Expr2 Expr2
           | Equal Expr2 Expr2
           | And Expr2 Expr2
           | Or Expr2 Expr2
           | LitInt Int
           | LitBool Bool
           deriving Show

-- Tipo de dados para representar o resultado da expressão
data Result2 = IntVal Int
             | BoolVal Bool
             | Error String
             deriving (Eq, Show)

-- Funções auxiliares
applyInt :: (Int -> Int -> Int) -> Result2 -> Result2 -> Result2
applyInt f (IntVal x) (IntVal y) = IntVal (f x y)
applyInt _ _ _ = Error "Operação inválida sobre inteiros"

applyIntBool :: (Int -> Int -> Bool) -> Result2 -> Result2 -> Result2
applyIntBool f (IntVal x) (IntVal y) = BoolVal (f x y)
applyIntBool _ _ _ = Error "Operação inválida sobre comparação"

applyBool :: (Bool -> Bool -> Bool) -> Result2 -> Result2 -> Result2
applyBool f (BoolVal x) (BoolVal y) = BoolVal (f x y)
applyBool _ _ _ = Error "Operação inválida sobre booleanos"

safeDiv2 :: Result2 -> Result2 -> Result2
safeDiv2 (IntVal _) (IntVal 0) = Error "Divisão por zero"
safeDiv2 (IntVal x) (IntVal y) = IntVal (x `div` y)
safeDiv2 _ _ = Error "Operação inválida na divisão"

-- Função para avaliar uma expressão
eval2 :: Expr2 -> Result2
eval2 (LitInt n)   = IntVal n
eval2 (LitBool b)  = BoolVal b
eval2 (Sum e1 e2)  = applyInt (+) (eval2 e1) (eval2 e2)
eval2 (Sub e1 e2)  = applyInt (-) (eval2 e1) (eval2 e2)
eval2 (Mult e1 e2) = applyInt (*) (eval2 e1) (eval2 e2)
eval2 (Div e1 e2)  = safeDiv2 (eval2 e1) (eval2 e2)
eval2 (Less e1 e2) = applyIntBool (<) (eval2 e1) (eval2 e2)
eval2 (Equal e1 e2) = case (eval2 e1, eval2 e2) of
    (IntVal x, IntVal y)   -> BoolVal (x == y)
    (BoolVal x, BoolVal y) -> BoolVal (x == y)
    _ -> Error "Comparação inválida"
eval2 (And e1 e2) = applyBool (&&) (eval2 e1) (eval2 e2)
eval2 (Or e1 e2)  = applyBool (||) (eval2 e1) (eval2 e2)

-- Geração de expressões a partir de lista mista
data Elem = EInt Int | EBool Bool
    deriving Show

expressions2 :: [Elem] -> [Expr2]
expressions2 []  = []
expressions2 [EInt n]  = [LitInt n]
expressions2 [EBool b] = [LitBool b]
expressions2 ns =
  [ op l r
  | (ls, rs) <- splits ns
  , l <- expressions2 ls
  , r <- expressions2 rs
  , op <- [Sum, Sub, Mult, Div, Less, Equal, And, Or]
  ]

-- Função para dividir uma lista em duas partes não vazias
splits :: [a] -> [([a], [a])]
splits xs = go [] xs
  where
    go _ []     = []
    go _ [_]    = []
    go prefix (x:suffix) = (reverse (x:prefix), suffix) : go (x:prefix) suffix

-- Função para formatar uma expressão como string
formatExpr2 :: Expr2 -> String
formatExpr2 (LitInt n)   = show n
formatExpr2 (LitBool b)  = show b
formatExpr2 (Sum e1 e2)  = "(" ++ formatExpr2 e1 ++ " + "  ++ formatExpr2 e2 ++ ")"
formatExpr2 (Sub e1 e2)  = "(" ++ formatExpr2 e1 ++ " - "  ++ formatExpr2 e2 ++ ")"
formatExpr2 (Mult e1 e2) = "(" ++ formatExpr2 e1 ++ " * "  ++ formatExpr2 e2 ++ ")"
formatExpr2 (Div e1 e2)  = "(" ++ formatExpr2 e1 ++ " / "  ++ formatExpr2 e2 ++ ")"
formatExpr2 (Less e1 e2) = "(" ++ formatExpr2 e1 ++ " < "  ++ formatExpr2 e2 ++ ")"
formatExpr2 (Equal e1 e2)= "(" ++ formatExpr2 e1 ++ " == " ++ formatExpr2 e2 ++ ")"
formatExpr2 (And e1 e2)  = "(" ++ formatExpr2 e1 ++ " && " ++ formatExpr2 e2 ++ ")"
formatExpr2 (Or e1 e2)   = "(" ++ formatExpr2 e1 ++ " || " ++ formatExpr2 e2 ++ ")"

-- Função principal
solution2 :: [Elem] -> [(String, String)]
solution2 elems = do
    (leftElems, rightElems) <- splits elems
    leftExpr  <- expressions2 leftElems
    rightExpr <- expressions2 rightElems
    let r1 = eval2 leftExpr
    let r2 = eval2 rightExpr
    if r1 == r2 
      then return (formatExpr2 leftExpr, formatExpr2 rightExpr)
    else []

main :: IO ()
main = do
    let list = [EInt 2, EInt 2, EBool False]
    let results = solution2 list
    putStrLn $ "Encontradas " ++ show (length results) ++ " soluções para a lista " ++ show list
    mapM_ (\(l, r) -> putStrLn $ l ++ " = " ++ r) results
