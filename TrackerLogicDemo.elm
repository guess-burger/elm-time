import Graphics.Element exposing (show)
import Regex
import List
import String
import Result
import Dict

elements =
  ["0900 Ticket A", "1061 coffee", "1200 Lunch", "1230 Ticket A","14000 Thing", "1700","","1800 ", "2400 Home"]


split a =
   Regex.split (Regex.AtMost 1) (Regex.regex " ") a

maybe_two : List a -> Maybe (a, a)
maybe_two n =
  case n of
    [] ->
      Nothing
    time :: name :: [] ->
      Just (time, name)
    time :: other ->
      Nothing


valid_time_length (time, name) =
  if String.length time == 4
  then Just (time, name)
  else Nothing

numeric_min_hours x =
  let
    (time, name) = x
    hours = String.left 2 time
    mins = String.right 2 time
    num_hours_result = String.toInt hours
    num_mins_result = String.toInt mins
  in
    case (num_hours_result, num_mins_result) of
      (Ok num_hours, Ok num_mins) -> 
        Just (num_hours, num_mins, name)
      _ ->
        Nothing

valid_time_values x =
  let
    (hr, min, name) = x
  in
    if(hr >= 0 && hr < 24 && min >= 0 && min < 60)
    then Just(hr, min, name)
    else Nothing
    

maybe_valid_time =
  split >> thing

thing a = 
   maybe_two a 
     `Maybe.andThen` valid_time_length
     `Maybe.andThen` numeric_min_hours
     `Maybe.andThen` valid_time_values


zip_self list =
  case list of
    hl::tl ->
      List.map2 (\a b -> (a, b)) list tl
    [] ->
      []

to_minutes x = 
  let 
    (before, after) = x
    (b_hr, b_min, b_name) = before
    (a_hr, a_min, _) = after
    mins = (a_hr - b_hr) * 60 + a_min - b_min
  in
    (b_name, mins)

time_collect x dict = 
  let
    (name, mins) = x
  in
    case Dict.get name dict of
      Just total -> 
        Dict.insert name (total+mins) dict
      Nothing ->
        Dict.insert name mins dict

format_time x = 
  let
    (name, total_mins) = x
    hrs = total_mins // 60
    mins = total_mins % 60
    string_hrs = toString hrs
    string_mins = toString mins
  in
    string_hrs ++ "h " ++ string_mins ++ "m - " ++ name 

main =
  elements
    |> List.filterMap maybe_valid_time
    |> zip_self
    |> List.map to_minutes
    |> List.foldl time_collect Dict.empty
    |> Dict.toList
    |> List.map format_time 
    |> show
