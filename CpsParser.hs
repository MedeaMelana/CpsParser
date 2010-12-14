{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeOperators #-}

module CpsParser where

import Prelude hiding (id, (.))
import Control.Category
import qualified Control.Applicative as A
import Control.Applicative ((<$>), (<*>), (<*), (<$), (<|>))
import qualified Text.Parsec as P
import Data.Monoid

newtype CpsParser b a = CpsParser (P.Parsec String () (a -> b))

instance Category CpsParser where
  id = CpsParser (A.pure id)
  CpsParser p . CpsParser q = CpsParser $ flip (.) <$> p <*> q

infixr 9 .~
(.~) :: CpsParser a b -> CpsParser b c -> CpsParser a c
CpsParser p .~ CpsParser q = CpsParser $ (.) <$> p <*> q

instance Monoid (CpsParser a b) where
  mempty = CpsParser A.empty
  CpsParser p `mappend` CpsParser q = CpsParser $ P.try p <|> q

infixl 3 <>
(<>) :: Monoid a => a -> a -> a
(<>) = mappend


-- Some useful parsers

int :: CpsParser r (Int -> r)
int = CpsParser $ (\s k -> k (read s)) <$> P.many1 P.digit

digit :: CpsParser r (Int -> r)
digit = CpsParser $ (\c k -> k (read [c])) <$> P.digit

str :: String -> CpsParser r r
str s = CpsParser $ id <$ P.string s

runId :: CpsParser b (a -> a) -> String -> b
runId p s = run p s id

run :: CpsParser b a -> String -> a -> b
run (CpsParser p) s =
  case P.runParser (p <* P.eof) () "" s of
    Right res -> res
    Left err -> error (show err)

push :: a -> CpsParser r (a -> r)
push x = pure ($ x)

discard :: CpsParser (a -> r) r
discard = pure const

pure :: (a -> b) -> CpsParser b a
pure f = CpsParser (A.pure f)

satisfy :: (Char -> Bool) -> CpsParser r (Char -> r)
satisfy f = CpsParser $ (\c k -> k c) <$> P.satisfy f

opt :: CpsParser r r -> CpsParser r r
opt = (<> id)

manyr :: CpsParser r r -> CpsParser r r
manyr = opt . somer

somer :: CpsParser r r -> CpsParser r r
somer p = p . manyr p

chainr1 :: (forall r. CpsParser r (a -> r)) -> (forall r. CpsParser (a -> a -> r) (a -> r)) -> CpsParser r (a -> r)
chainr1 p op = manyr (p .~ op) . p

manyl :: CpsParser r r -> CpsParser r r
manyl = opt . somel

somel :: CpsParser r r -> CpsParser r r
somel p = p .~ manyl p

char :: CpsParser r (Char -> r)
char = satisfy (const True)

plus :: CpsParser (Int -> Int -> r) (Int -> r)
plus = pure (\k x y -> k (x + y)) . str "+"

minus :: CpsParser (Int -> Int -> r) (Int -> r)
minus = pure (\k x y -> k (x - y)) . str "-"

neg :: CpsParser (Int -> r) (Int -> r)
neg = pure (\k x -> k (- x)) . str "-"

pos :: CpsParser (Int -> r) (Int -> r)
pos = pure (\k x -> k x) . str "+"



data Expr = Num Int | Add Expr Expr
  deriving (Eq, Show)

num :: CpsParser (Int -> r) (Expr -> r)
num = pure (\k n -> k (Num n))

add :: CpsParser (Expr -> Expr -> r) (Expr -> r)
add = pure (\k x y -> k (Add x y))
