
arrayLength :: [a] -> Integer
arrayLength []  = 0
arrayLength [_] = 1
arrayLength [_,_] = 2

data Expr = Val Int | Add Expr Expr
                    | Mult Expr Expr
                    deriving (Show)

eval            :: Expr -> Int
eval (Val n)    = n
eval (Add x y)  = eval x + eval y
eval (Mult x y) = eval x * eval y


parseExpr :: [String] -> Expr
parseExpr []



1*2+3/4

