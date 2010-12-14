{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeOperators #-}

module CpsParser where

import Prelude hiding (id, (.))
import Control.Category
import Control.Applicative
import Control.Monad
import qualified Text.Parsec as P
import Data.Monoid

newtype CpsParser b a = CpsParser (P.Parsec String () (a -> b))

instance Category CpsParser where
  id = CpsParser (pure id)
  CpsParser p . CpsParser q = CpsParser $ flip (.) <$> p <*> q

infixr 9 .~
(.~) :: CpsParser a b -> CpsParser b c -> CpsParser a c
CpsParser p .~ CpsParser q = CpsParser $ (.) <$> p <*> q

instance Monoid (CpsParser a b) where
  mempty = CpsParser empty
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

infixl 4 `ɟmap`
ɟmap :: (b -> a) -> CpsParser r a -> CpsParser r b
ɟmap f (CpsParser p) = CpsParser $ (. f) <$> p

runId :: CpsParser b (a -> a) -> String -> b
runId p s = run p s id

run :: CpsParser b a -> String -> a -> b
run (CpsParser p) s =
  case P.runParser (p <* P.eof) () "" s of
    Right res -> res
    Left err -> error (show err)

cpure :: a -> CpsParser r (a -> r)
cpure x = ($ x) `ɟmap` id

csatisfy :: (Char -> Bool) -> CpsParser r (Char -> r)
csatisfy f = CpsParser $ (\c k -> k c) <$> P.satisfy f

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
char = csatisfy (const True)

plus :: CpsParser (Int -> Int -> r) (Int -> r)
plus = (\k x y -> k (x + y)) `ɟmap` id

minus :: CpsParser (Int -> Int -> r) (Int -> r)
minus = (\k x y -> k (x - y)) `ɟmap` str "-"

neg :: CpsParser (Int -> r) (Int -> r)
neg = (\k x -> k (- x)) `ɟmap` str "-"

pos :: CpsParser (Int -> r) (Int -> r)
pos = (\k x -> k x) `ɟmap` str "+"

chainr :: (forall r. CpsParser r (a -> r)) -> (forall r. CpsParser (a -> a -> r) (a -> r)) -> CpsParser r (a -> r)
-- chainr :: (forall r. CpsParser r (a -> b -> r)) -> (forall r. CpsParser (a -> b -> a -> b -> r) (a -> b -> r)) -> CpsParser r (a -> b -> r)
chainr x op = x .~ cmany (op . x)

cmany :: CpsParser r r -> CpsParser r r
-- cmany :: (Category (~>), Monoid (r ~> r)) => r ~> r -> r ~> r
cmany p = p . cmany p <> id




data Expr = Num Int | Add Expr Expr
  deriving (Eq, Show)

num = (\k n -> k (Num n)) `ɟmap` int
add = (\k y x -> k (Add x y)) `ɟmap` str "+"
