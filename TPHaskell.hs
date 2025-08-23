data Expr = Sum Expr Expr
          | Mult Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Lit Int
          | Var String
          deriving show
