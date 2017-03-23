module Auxiliaries exposing ( .. )

import Basics exposing ( identity, toFloat )
import List exposing   ( foldr, sum, map )

{- Flatten the Maybes in a list, so that Nothings are removed and Justs are unwrapped. -}

catMaybes : List (Maybe a) -> List a
catMaybes = 
  let f m = case m of
    Nothing -> identity
    Just x  -> (::) x
  in foldr f []

-- Truncate a floating point after a certain amount of of positions.
truncateAt : Int -> Float -> Float
truncateAt pos x = 
  if (pos < 0) then x
  else let power = 10 ^ toFloat pos
       in toFloat (round (power * x)) / power

{- A precise division of integers. -}
(///) : Int -> Int -> Float
(///) a b = toFloat a / toFloat b

{- Sum all values obtained by applying the given function to every element in the list. -}
sumMap : (a -> number) -> List a -> number
sumMap f = sum << map f