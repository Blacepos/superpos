module Main (main) where

import Superpos

main :: IO ()
main = print (getSuperpos flip3Times)

data CoinFlip = Heads | Tails deriving(Show)

flipResult :: Superpos Double CoinFlip
flipResult = Superpos [(Heads, 1), (Tails, 1)]

flip3Times :: Superpos Double (CoinFlip, CoinFlip, CoinFlip)
flip3Times = do
    a <- flipResult -- `a` is "both" a Heads and a Tails, so all operations done on it will happen on each individually
    b <- flipResult
    c <- flipResult
    return (a, b, c)
