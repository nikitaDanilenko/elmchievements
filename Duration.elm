module Duration exposing ( .. )

import Auxiliaries exposing ( zip )

{- A representation of time in terms of hours and minutes. -}
type alias Duration = { hours : Int, minutes : Int }

{- Interpret an integer as a number of minutes. Negative numbers are considered to be zero. -}
toDuration : Int -> Duration
toDuration n = if (n < 0) then Duration 0 0 else Duration (n // 60) (n % 60)

{- Pretty-print a duration as a combination of hours and minutes. -}
toString : Duration -> String
toString d = 
  String.join " " 
       ([Basics.toString d.hours, "hours"] 
        ++ (if d.minutes == 0 then [] else [Basics.toString d.minutes, "minutes"]))

toFullString : Duration -> String
toFullString d =
  let (days, remHours) = (d.hours // 24, d.hours % 24)
      (weeks, remDays) = (days // 7, days % 7)
      times = zip [weeks, remDays, remHours, d.minutes] [" weeks", " days", " hours", " minutes"]
      filtered = List.filter (\(t, _) -> t > 0) times
  in String.concat (List.map (\(t, n) -> String.concat [Basics.toString t, n]) filtered)

{- Convert a duration into a number of minutes. -}
toInt : Duration -> Int
toInt d = d.hours * 60 + d.minutes

{- Compute the sum of a list of durations. -}
sum : List Duration -> Duration
sum = toDuration << List.sum << List.map toInt

{- Zero minutes expressed as a Duration. -}
zero : Duration
zero = toDuration 0