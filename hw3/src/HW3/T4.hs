module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where


import HW3.T1
import Control.Monad(ap)

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f oldState = S $ \s -> let (a :# newState) = runS oldState s
                                in (f a) :# newState

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState oldState = S $ \s -> let (S a :# newState) = runS oldState s
                               in a newState

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# (f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) = ap

instance Monad (State s) where
  m >>= f = joinState (fmap f m) 

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

evalBinary :: Expr -> 
              Expr -> 
              (Double -> Double -> Prim Double) ->
              (Double -> Double -> Double) ->
              State [Prim Double] Double
evalBinary x y constructor op = do
  xValue <- eval x
  yValue <- eval y
  modifyState $ \s -> constructor xValue yValue : s
  return $ xValue `op` yValue

evalUnary :: Expr ->
             (Double -> Prim Double) ->
             (Double -> Double) ->
             State [Prim Double] Double
evalUnary x constructor op = do
  xValue <- eval x
  modifyState $ \s -> constructor xValue : s
  return $ op xValue 

eval :: Expr -> State [Prim Double] Double
eval (Val x) = return x
eval (Op (Add x y)) = evalBinary x y Add (+)
eval (Op (Sub x y)) = evalBinary x y Sub (-)
eval (Op (Mul x y)) = evalBinary x y Mul (*)
eval (Op (Div x y)) = evalBinary x y Div (/)
eval (Op (Abs x)) = evalUnary x Abs abs
eval (Op (Sgn x)) = evalUnary x Sgn signum