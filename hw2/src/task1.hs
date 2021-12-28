module Task1 where 

data Expr = Const Int | BinaryOperation Operator Expr Expr

data Operator = Add | Sub | Mul | Div | Pow

data ArithmeticError = DivByZero | PowByNeg
    deriving (Show, Eq)

const :: Int -> Expr
const = Const

add :: Expr -> Expr -> Expr
add = BinaryOperation Add

sub :: Expr -> Expr -> Expr
sub = BinaryOperation Sub

mul :: Expr -> Expr -> Expr
mul = BinaryOperation Mul

div :: Expr -> Expr -> Expr
div = BinaryOperation Div

pow :: Expr -> Expr -> Expr
pow = BinaryOperation Pow

apply :: Operator -> Int -> Int -> Either ArithmeticError Int
apply Add l r = Right (l + r)
apply Sub l r = Right (l - r)
apply Mul l r = Right (l * r)
apply Div _ 0 = Left DivByZero
apply Div l r = Right (l `quot` r)
apply Pow l r | r < 0 = Left PowByNeg
              | otherwise = Right (l ^ r)

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (BinaryOperation f l r) = eval l >>= \lv -> eval r >>= apply f lv

bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = bin (n - 1) >>= \b -> [0 : b, 1 : b]








