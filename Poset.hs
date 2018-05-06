{- Poset.hs

   About: Code to compute a product poset, to answer exercise 1.41 in the book
          Seven Sketches in Compositionality: An Invitation to Applied Category Theory
          (Brendan Fong, David I. Spivak)

-}

import Data.List (delete)
import Prelude hiding (product)

-- A poset is a list of nodes and a list of a <= b relations.  Transitivity is
-- implemented computationally, so explicitly listing compositions is not necessary
data Relation a = Relation a a
                  deriving (Eq, Show)

data Poset a = Poset [a] [Relation a]
               deriving (Show)

poset1 :: Poset Char
poset1 = Poset ['a','b','c'] [Relation 'a' 'b', Relation 'a' 'c']

poset2 :: Poset Int
poset2 = Poset [1,2] [Relation 1 2]

product :: (Eq a, Eq b) => Poset a -> Poset b -> Poset (a, b)
product p1@(Poset ob1 rel1) p2@(Poset ob2 rel2) = Poset objects relation
  where objects  = [ (o1, o2) | o1 <- ob1,
                                o2 <- ob2 ]
        relation = [ Relation (a, b) (a', b') | (a, b)   <- objects,
                                                (a', b') <- objects,
                                                havePath rel1 a a',
                                                havePath rel2 b b'
                                                ]

-- Because poset relations are transitive, and we don't explicitly list compositions
-- in our list of relations, checking only for (a, b) isn't enough, we have to
-- recursively walk the nodes to find whether a <= b along multiple arrows
--
-- Note there is no "Ord a" type constraint on this function (or anywhere in this
-- file), since whether a <= b isn't really about comparison on the underlying
-- data type, rather the existence of a path through the list of relations,
-- which only requires matching source and target objects (hence the Eq constraint)
havePath :: Eq a => [Relation a] -> a -> a -> Bool
havePath _ a b | a == b = True
havePath relation a b   = walk a b relation
  where walk a b rel = any (\(Relation x y) -> a == x &&
                                               ( b == y
                                                 || walk y b (delete (Relation x y) rel)
                                               )
                           ) rel

-- Output a graph to paste into http://www.webgraphviz.com/
-- Doesn't render vertices of degree 0
graphviz :: (Eq a, Show a) => Poset a -> String
graphviz (Poset _ relation) = "digraph ProductPoset {\n" ++ nodes ++ "}"
  where nodes               = unlines $ map edge nonidentities
        edge (Relation a b) = "  \"" ++ show a ++ "\" -> \"" ++ show b ++ "\""
        nonidentities       = filter (\(Relation a b) -> a /= b) relation

{-
    *Main> putStrLn $ graphviz $ product poset1 poset2
    digraph ProductPoset {
      "('a',1)" -> "('a',2)"
      "('a',1)" -> "('b',1)"
      "('a',1)" -> "('b',2)"
      "('a',1)" -> "('c',1)"
      "('a',1)" -> "('c',2)"
      "('a',2)" -> "('b',2)"
      "('a',2)" -> "('c',2)"
      "('b',1)" -> "('b',2)"
      "('c',1)" -> "('c',2)"
    }
-}
