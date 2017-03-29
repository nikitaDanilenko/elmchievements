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

{- Zip lists by applying a given function to the respective positions in the list.
   The length of the result list is the length of the shorter list. -}
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f = let go l m = case (l, m) of
                             (x :: xs, y :: ys) -> f x y :: go xs ys
                             (_,       _)       -> []
            in go

{- Combine two lists into a list of pairs.
   The length of the result is the length of the shorter list. -}
zip : List a -> List b -> List (a, b)
zip = zipWith (,)

{- Flatten a list of words to a sentence by interspersing an empty space in between words and
   concatenating the result. -}
unwords : List String -> String
unwords = String.concat << List.intersperse " "