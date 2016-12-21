module Auxiliaries exposing ( .. )

import List exposing ( foldr )

{- Flatten the Maybes in a list, so that Nothings are removed and Justs are unwrapped. -}

catMaybes : List (Maybe a) -> List a
catMaybes = 
  let f m = case m of
    Nothing -> Basics.identity
    Just x  -> (::) x
  in foldr f []