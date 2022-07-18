{-# LANGUAGE LambdaCase #-}
module Main where

type Ident = String

data Expr = Number Int
          | Boolean Bool
          | Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | If Expr Expr Expr
          | Equals Expr Expr
          | Var Ident
          | Let Defn Expr
          | Lam [Ident] Expr
          | Apply Expr [Expr]
          deriving (Show, Eq)


-- fib 10 :: Apply (Lam ["n"] ......) []
-- \x y -> x + y :: Lam ["x", "y"] (Plus Var "x" var "y")

data Value = NumVal Int | BoolVal Bool | Closure [Ident] Expr Env
   deriving (Show, Eq)

data Defn = Val Ident Expr | Rec Ident Expr deriving (Show, Eq)

type Env = [(Ident, Value)]

newtype Id a = Id a

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance Applicative Id where
  pure a = Id a
  (<*>) = \(Id f) (Id x) -> Id (f x)

instance Monad Id where
  (>>=) = \(Id x) f -> f x

type M = Id

eval :: Env -> Expr -> M Value

-- Numbers
eval env (Number  i   ) = return $ NumVal i

-- Booleans
eval env (Boolean bool) = return $ BoolVal bool

-- Addition
eval env (Plus e1 e2) = do
  ~(NumVal n1) <- eval env e1
  ~(NumVal n2) <- eval env e2
  return $ NumVal (n1 + n2)

-- Substraction.
eval env (Minus e1 e2) = do
  ~(NumVal n1) <- eval env e1
  ~(NumVal n2) <- eval env e2
  return $ NumVal (n1 - n2)

-- Multiplication.
eval env (Mult e1 e2) = do
  ~(NumVal n1) <- eval env e1
  ~(NumVal n2) <- eval env e2
  return $ NumVal (n1 * n2)

-- If statements.
eval env (If pred tru fal) = eval env pred >>= \case
  ~(BoolVal True ) -> eval env tru
  ~(BoolVal False) -> eval env fal
  _               -> error "Expected a boolean value."


-- Comparision
eval env (Equals e1 e2) = BoolVal <$> ((==) <$> eval env e1 <*> eval env e2)

-- Variables
eval env (Var i       ) = return $ find env i

-- Let statements
eval env (Let   d   e1) = elab env d >>= \environment -> eval environment e1 

-- Lambdas / Functions.
eval env (Lam   ids e ) = return $ Closure ids e env

-- Function application
eval env (Apply f   xs) = do
   f' <- eval env f
   xs' <- mapM (eval env) xs
   apply f' xs'

find env i = snd $ head $ filter (\(i', _) -> i == i') env

elab :: Env -> Defn -> M Env
elab env (Val i e           ) = eval env e >>= \e' -> return $ (i, e'):env
elab env (Rec i l@(Lam _ _)) = env'
  where env' = (env' >>= \environment -> eval environment l) >>= \e' -> return $ (i, e'): env
elab _ _ = error "Only lambdas can be recursive."



apply :: Value -> [Value] -> M Value
apply (Closure ids e env) vals = eval (zip ids vals ++ env) e
apply _                   _    = error "Your Mom."

main = putStrLn "This is an interpreter apparently, idk."
