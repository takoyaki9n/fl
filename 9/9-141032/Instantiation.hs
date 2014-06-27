module Instantiation where

data Suit = Club | Diamond | Heart | Spade
data Card = Card Suit Int | Joker
data LList a = Nil | Cons a (LList a)
data BT a = L a | B (BT a) (BT a)

instance Eq Suit where
  Club    == Club    = True
  Diamond == Diamond = True
  Heart   == Heart   = True
  Spade   == Spade   = True
  _         == _     = False

instance Ord Suit where
  Club    <= Club    = True
  Club    <= Diamond = True
  Club    <= Heart   = True
  Club    <= Spade   = True
  Diamond <= Diamond = True
  Diamond <= Heart   = True
  Diamond <= Spade   = True
  Heart   <= Heart   = True
  Heart   <= Spade   = True
  Spade   <= Spade   = True
  _       <= _       = False

instance Show Suit where
  show Club    = "♣"
  show Diamond = "♦"
  show Heart   = "♥"
  show Spade   = "♠"

instance Eq Card where
  Card s n  == Card t m  = (s == t) && (n == m)
  Joker     == Joker     = True
  _         == _         = False

instance Ord Card where
  Card s n  <= Card t m  = if n == m then s <= t else n <= m 
  _         <= Joker     = True
  _         <= _         = False

instance Show Card where
  show (Card s n)    = show s ++ " " ++ show n
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
