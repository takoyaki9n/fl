module Parser where

import Control.Applicative
import Data.Char
import Interpreter

newtype Parser a = Parser {parse :: String -> [(a, String)]}
instance Functor Parser where
  fmap f (Parser p) = 
    Parser $ \s -> map (\(a, t) -> (f a, t)) (p s)
instance Monad Parser where
  return a = Parser $ \s -> [(a, s)]
  Parser p >>= f = 
    Parser $ \s -> concat [parse (f a) t | (a, t) <- (p s)]
  fail _ = Parser $ \_ -> []
instance Applicative Parser where
  pure = return
  Parser f <*> Parser p = 
    Parser (\s -> [(c a, u) | (c, t) <- f s, (a, u) <- p t])

(+++) :: Parser a -> Parser a -> Parser a
a +++ b = 
  Parser (\s ->  parse a s ++ parse b s)

char :: Char -> Parser Char
char c = Parser $ parseChar
  where 
    parseChar ""    = []
    parseChar (d:s) = if c == d then [(c, s)] else []

string :: String -> Parser String
string "" = return ""
string (c:s) = char c >> string s >> return (c:s)

longest :: Parser a -> Parser a
longest (Parser p) = 
  Parser $ \s -> 
  case p s of
    [] -> []
    xs -> [foldl1 (\x y -> if (length $ snd x) <= (length $ snd y) then x else y) xs]

complete :: Parser a -> Parser a
complete (Parser p) = 
  Parser $ \s -> filter (\(a, t) -> length t == 0) (p s)

number :: Parser Int
number = 
  longest (zeroDigit
           +++ 
           (do xs <- nonZeroNumber; return $ foldl (\e x -> 10 * e + x) 0 xs))
  where
    nonZeroNumber :: Parser [Int]
    nonZeroNumber = 
      (do x <- nonZeroDigit; return [x])
      +++
      (do x <- nonZeroDigit; xs <- digits; return (x:xs))
    digits :: Parser [Int]
    digits = 
      (do x <- digit; return [x])
      +++
      (do x <- digit; xs <- digits; return (x:xs))
    nonZeroDigit :: Parser Int
    nonZeroDigit = foldr1 (+++) (map (\c -> digitToInt <$> (char c)) ['1'..'9'])
    zeroDigit :: Parser Int
    zeroDigit = digitToInt <$> (char '0')
    digit :: Parser Int
    digit = zeroDigit +++ nonZeroDigit

bool :: Parser Bool
bool = 
  (do string "True"; return True)
  +++
  (do string "False"; return False)

space :: Parser ()
space = 
  longest ((wschar >> return ())
           +++
           (wschar >> space >> return ()))
  where
    wschar :: Parser Char
    wschar = foldr1 (+++) (map char [' ', '\t', '\n', '\r', '\0'])

var :: Parser String
var = 
  longest (do c <- alpha; s <- alphaDigits; return (c:s))
  where
    alphaDigits :: Parser String
    alphaDigits = 
      (do return "")
      +++
      (do c <- alphaDigit; s <- alphaDigits; return (c:s))
    alpha :: Parser Char
    alpha = foldr1 (+++) (map char (['a'..'z'] ++  ['A'..'Z']))
    alphaDigit :: Parser Char
    alphaDigit = alpha +++ (foldr1 (+++) (map char ['0'..'9']))

token p = p +++ (do a <- p; space; return a)
symb s = token (string s)

start = complete ((space >> expr) +++ expr)

expr = 
  (do symb "let"
      n <-  token var
      symb "="
      e1 <- expr
      symb "in"
      e2 <- expr
      return $ ELet n e1 e2)
  +++
  (do symb "if"
      e1 <- expr 
      symb "then"
      e2 <- expr
      symb "else"
      e3 <- expr
      return $ EIf e1 e2 e3)
  +++
  bool_arith_expr

bool_arith_expr = 
  (do e1 <- arith_expr
      symb "==" 
      e2 <- arith_expr
      return $ EEq e1 e2)
  +++
  (do e1 <- arith_expr
      symb "<" 
      e2 <- arith_expr
      return $ ELt e1 e2)
  +++
  arith_expr

arith_expr = 
  do e1 <- factor_expr
     e2 <- arith_expr' 
     return (e2 e1)

arith_expr' = 
  (do symb "+"
      e1 <- factor_expr
      e2 <- arith_expr'
      return $ (`EAdd` e1) . e2)
  +++
  (do symb "-"
      e1 <- factor_expr
      e2 <- arith_expr'
      return $ (`ESub` e1) . e2)
  +++
  return id

factor_expr = 
  do e1 <- simple_expr
     e2 <- factor_expr'
     return (e2 e1)

factor_expr' = 
  (do symb "*"
      e1 <- simple_expr
      e2 <- factor_expr'
      return $ (`EMul` e1). e2)
  +++
  (do symb "/"
      e1 <- simple_expr
      e2 <- factor_expr'
      return $ (`EDiv` e1) . e2)
  +++
  return id

simple_expr = 
  (do n <- token var
      return (EVar n))
  +++
  (do x <- token number
      return (EConst (VInt x)))
  +++
  (do x <- token bool
      return (EConst (VBool x)))
  +++
  (do symb "("
      e <- expr
      symb ")"
      return e)
  +++
  (do symb "-"
      e <- simple_expr
      return (ESub (EConst (VInt 0)) e))
