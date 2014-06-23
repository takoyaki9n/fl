module Instantiation where

data Card = Club Int | Diamond Int | Heart Int | Spade Int | Joker
data LList a = Nil | Cons a (LList a)
data BT a = L a | B (BT a) (BT a)

instance Eq Card where
  Club a    == Club b    = a == b
  Diamond a == Diamond b = a == b
  Heart a   == Heart b   = a == b
  Spade a   == Spade b   = a == b
  Joker     == Joker     = True
  _         == _         = False

instance Ord Card where
  Club a    <= Club b     = a <= b
  Club _    <= _          = True
  Diamond a <=  Diamond b = a <= b
  Diamond _ <= _          = True
  Heart a   <= Heart b    = a <= b
  Heart _   <= _          = True
  Spade a  <= Spade b     = a <= b
  Spade _  <= _           = True
  Joker    <= Joker       = True
  _        <= _           = False

instance Show Card where
  show (Club a)    = "♣ " ++ show a
  show (Diamond a) = "♦ " ++ show a
  show (Heart a)   = "♥ " ++ show a
  show (Spade a)   = "♠ " ++ show a
  show Joker       = "Joker"

instance (Eq a) => Eq (LList a) where
  Nil       == Nil       = True
  Cons x xs == Cons y ys = (x == y) && (xs == ys)
  _         == _         = False

instance (Show a) => Show (LList a) where 
  show Nil = ""
  show (Cons x Nil) = show x
  show (Cons x xs)  = show x ++ "::" ++ show xs

instance (Eq a) => Eq (BT a) where 
  L a       == L b        =  a == b
  (B l1 r1) == (B l2 r2)  =  (l1 == l2) && (r1 == r2)
  _              == _               =  False

instance (Show a) => Show (BT a) where 
  show (L a)   = show a
  show (B l r) =  "(" ++ show l ++ ", " ++ show r ++ ")"
