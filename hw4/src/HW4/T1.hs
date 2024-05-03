module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import Control.Monad(ap)

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f es = ES $ mapExcept (mapAnnotated f) . runES es

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> wrapExcept (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState es = ES $ \s -> case runES es s of
                                Error e -> Error e
                                Success (es2 :# s2) -> runES es2 s2

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success $ () :# f s

throwExceptState :: e -> ExceptState e s a
throwExceptState = ES . return . Error

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m) 

data EvaluationError = DivideByZero
  deriving Show

evalBinary :: Expr -> 
              Expr -> 
              (Double -> Double -> Prim Double) ->
              (Double -> Double -> ExceptState EvaluationError [Prim Double] Double) ->
              ExceptState EvaluationError [Prim Double] Double
evalBinary x y constructor op = do
  xValue <- eval x
  yValue <- eval y
  modifyExceptState $ \s -> constructor xValue yValue : s
  xValue `op` yValue

evalUnary :: Expr ->
             (Double -> Prim Double) ->
             (Double -> Double) ->
             ExceptState EvaluationError [Prim Double] Double
evalUnary x constructor op = do
  xValue <- eval x
  modifyExceptState $ \s -> constructor xValue : s
  return $ op xValue 

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = return x
eval (Op (Add x y)) = evalBinary x y Add (\a b -> return $ a + b)
eval (Op (Sub x y)) = evalBinary x y Sub (\a b -> return $ a - b)
eval (Op (Mul x y)) = evalBinary x y Mul (\a b -> return $ a * b)
eval (Op (Div x y)) = evalBinary x y Div (\num denum -> case denum of 
  0 -> throwExceptState DivideByZero
  _ -> return (num / denum))
eval (Op (Abs x)) = evalUnary x Abs abs
eval (Op (Sgn x)) = evalUnary x Sgn signum