-- Modeling Example 3.94 from the book Seven Sketches in Compositionality - 15 June 2018 Edition,
-- online at: http://math.mit.edu/~dspivak/teaching/sp18/

data Color = Red
           | Blue
           | Black
           deriving (Eq)

setX, setY :: [Integer]
setX = [1..6]
setY = [1..4]

f, g :: Integer -> Color
f i = case i of { 1 -> Red;   2 -> Blue; 3 -> Red;  4 -> Red; 5 -> Black; 6 -> Blue }
g i = case i of { 1 -> Black; 2 -> Red;  3 -> Blue; 4 -> Red }

cospanPullback :: Eq a => (s -> a) -> (t -> a) ->
                          [s]      -> [t]      ->
                          [(s, t)]
cospanPullback f g setx sety = [ (x, y) | x <- setx,
                                          y <- sety,
                                          f x == g y ]

{-
    *Main> cospanPullback f g [1..6] [1..4]
    [(1,2),(1,4),(2,3),(3,2),(3,4),(4,2),(4,4),(5,1),(6,3)]
-}
