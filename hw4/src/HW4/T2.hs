{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Scientific
import GHC.Natural
import Data.Maybe
import Data.Functor

import HW4.Types
import HW4.T1 (ExceptState(..))

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) s = mapExcept (\(x :# _) -> x) $ runES es (0, s) 

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser (Char)
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error $ ErrorAtPos pos

instance Alternative Parser where
  empty = parseError
  (P p1) <|> (P p2) = P $ ES $ \s -> case runES p1 s of
    Error _ -> runES p2 s
    Success a -> Success a

-- No metohds
instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) -> case s of
  [] -> Success $ () :# (pos + 1, s)
  _ -> Error $ ErrorAtPos pos

pDigit :: Parser Integer
pDigit = do
  c <- (safeDigitToInt <$> pChar) 
  case c of 
    Nothing -> parseError
    Just x -> return x
    where
      safeDigitToInt :: Char -> Maybe Integer
      safeDigitToInt c 
        | c >= '0' && c <= '9' = Just $ toInteger $ fromEnum c - fromEnum '0'
        | otherwise = Nothing

pInteger :: Parser (Integer, Int)
pInteger = do
  x <- some pDigit
  return $ (foldl' (\acc val -> acc * 10 + val) 0 x,
            length x)

pSymbol :: Char -> Parser ()
pSymbol c = void $ mfilter ((==) c) pChar 
   
pSpace :: Parser ()
pSpace = void $ many $ mfilter isSpace pChar

pVal :: Parser Expr
pVal = do
  pSpace
  (x, _) <- pInteger
  sepMaybe <- optional $ pSymbol '.'
  fracMaybe <- optional $ pInteger
  if isJust sepMaybe /= isJust fracMaybe
  then parseError
  else let (y, expLen) = fromMaybe (0, 0) fracMaybe
       in return $ Val $ toRealFloat $ scientific x 0 + scientific y (- expLen)

pBrackets :: Parser Expr
pBrackets = do
  pSpace
  pSymbol '('
  x <- pAddSub
  pSpace
  pSymbol ')'
  return x

pUnary :: Parser Expr
pUnary = pBrackets <|> pVal

pOpMulDiv :: Parser (a -> a -> Prim a)
pOpMulDiv = pMul <|> pDiv
  where
    pMul = pSymbol '*' $> Mul
    pDiv = pSymbol '/' $> Div

pOpAddSub :: Parser (a -> a -> Prim a)
pOpAddSub = pAdd <|> pSub
  where
    pAdd = pSymbol '+' $> Add
    pSub = pSymbol '-' $> Sub

pAddSubInf :: Expr -> Parser Expr
pAddSubInf l = do
  pSpace
  op <- pOpAddSub
  r <- pMulDiv
  pAddSubInf $ Op $ op l r
  <|> return l

pMulDivInf :: Expr -> Parser Expr
pMulDivInf l = do
  pSpace
  op <- pOpMulDiv
  r <- pUnary
  pMulDivInf $ Op $ op l r
  <|> return l

pMulDiv :: Parser Expr
pMulDiv = do
  pSpace
  l <- pUnary
  pMulDivInf l

pAddSub :: Parser Expr
pAddSub = do
  pSpace
  l <- pMulDiv
  pAddSubInf l

pExpr :: Parser Expr
pExpr = do
  pSpace
  x <- pAddSub
  pSpace
  pEof
  return x

parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpr