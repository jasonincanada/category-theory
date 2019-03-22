{- fib-histo.hs

   Find the nth Fibonacci number quickly using a histomorphism.

   This code is drawn almost entirely from the Recursion Schemes tutorials published
   on the Adventures in Uncertainty blog at this URL:

     https://blog.sumtypeofway.com/recursion-schemes-part-iv-time-is-of-the-essence/

   That post and the 3 prior are required reading to understand this code.  After
   building up to histomorphisms, the calculation of the nth Fibonacci number is
   left as an exercise for the reader.  I've re-typed all the machinery for the
   histomorphism and only had to come up with the "go" function at the bottom of
   this code.  All other code is from the blog, though I made sure to type the
   majority of it from understanding instead of rote copying.
-}

{-# Language DeriveFunctor #-}
{-# Language GeneralizedNewtypeDeriving #-}

-- Borrow some categorical operators to make histo cleaner
import Control.Arrow ((>>>), (&&&))

-- The fixed point of a functor
newtype Fix f = In { out :: f (Fix f) }

data Attr f a = Attr { attribute :: a
                     , hole      :: f (Attr f a)
                     } deriving (Functor)

type CVAlgebra f a = f (Attr f a) -> a


-- The histomorphism mechanism
histo :: Functor f => CVAlgebra f a -> Fix f -> a
histo alg = worker >>> attribute
  where
    worker = out >>> fmap worker >>> (alg &&& id) >>> mkAttr
    mkAttr (a, b) = Attr a b


-- Model the natural numbers
data Nat a = Zero
           | Next a
           deriving (Show, Functor)

-- Convert to/from regular non-negative integers and their functor representations
expand :: Int -> Fix Nat
expand 0 = In Zero
expand n = In (Next $ expand (n-1))

compress :: Nat (Attr Nat a) -> Int
compress Zero              = 0
compress (Next (Attr _ n)) = 1 + compress n


-- Return the nth Fibonacci number
fib :: Int -> Integer
fib n = histo go (expand n)
  where
    -- :: Nat (Attr Nat Integer) -> Integer
    go :: CVAlgebra Nat Integer
    go Zero              = 0
    go (Next (Attr 0 _)) = 1
    go curr@(Next attr)  = let given = compress curr
                               fib1  = fetch attr 0
                               fib2  = fetch attr 1
                           in  fib1 + fib2

    -- "Look back" n steps in the saved history of computations
    fetch :: Attr Nat a -> Int -> a
    fetch cache 0 = attribute cache
    fetch cache n = let Next inner = hole cache -- Unpeel a layer of computation history
                    in  fetch inner (n-1)


{-
  -- Get the first few fibs
  λ> [ (n, fib n) | n <- [0..10]]
  [(0,0),(1,1),(2,1),(3,2),(4,3),(5,5),(6,8),(7,13),(8,21),(9,34),(10,55)]

  -- Get the 1000th fib (this calculates instantly--sub-problem results are cached)
  λ> fib 1000
  43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
-}
