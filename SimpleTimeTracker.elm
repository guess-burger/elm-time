port module SimpleTimeTracker exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.App as App
import String
import Regex
import Dict
import Json.Decode exposing (Decoder, decodeValue, object2, tuple2, (:=), string, list, int, null)


main : Program (Maybe Json.Decode.Value)
main =
  App.programWithFlags
      { init = init
      , update = update
      , view = view
      , subscriptions = \_ -> Sub.none
      }


port setStorage : StorageModel -> Cmd msg


-- MODEL

type alias TimeGroup =
    { times:List (String, Int)
    , total:Int
    }

type alias Model =
    { text:String
    , nonWorkItems:List String
    , workTimes:TimeGroup
    , nonWorkTimes:TimeGroup
    }

type alias StorageModel =
    { text:String
    , nonWorkItems:List String
    }


emptyModel : Model
emptyModel =
    { text="", nonWorkItems=[], workTimes=emptyTimeGroup, nonWorkTimes=emptyTimeGroup }

emptyTimeGroup : TimeGroup
emptyTimeGroup =
    { times=[], total=0 }


init : Maybe Json.Decode.Value -> ( Model, Cmd Msg )
init savedModel =
    (decode savedModel , Cmd.none)

decode savedModel =
    case savedModel of
        Nothing ->
            emptyModel
        Just jsonModel ->
            case decodeValue decoder jsonModel of
                Ok {text, nonWorkItems} ->
                    calculate_times text { emptyModel | nonWorkItems=nonWorkItems }
                Err _ ->
                    emptyModel

decoder : Decoder StorageModel
decoder =
    object2 StorageModel
        ( "text" := string )
        ( "nonWorkItems" := Json.Decode.list string)


-- UPDATE

type Msg
    = Change String
    | Work String
    | NonWork String
    | Over String
    | Out String

update : Msg -> Model -> (Model, Cmd Msg)
update msg oldModel =
    case msg of
        Change newText ->
            let
                lines = String.split "\n" newText
                valid_time_entries = List.filterMap maybe_valid_time_entry lines
                converted_times = to_worked_times valid_time_entries
                total = List.foldl (\(_,b) c -> b+c) 0 converted_times
                cmd = store newText oldModel.nonWorkItems
            in
                ({ oldModel | text=newText, workTimes={ times=converted_times, total=total } }, cmd )
        Work item ->
            (oldModel, Cmd.none)
        NonWork item ->
            (oldModel, Cmd.none)
        Over item ->
            (oldModel, Cmd.none)
        Out item ->
            (oldModel, Cmd.none)

calculate_times newText model =
    -- Ignore nonWork for the tme being!
    let
        lines = String.split "\n" newText
        valid_time_entries = List.filterMap maybe_valid_time_entry lines
        converted_times = to_worked_times valid_time_entries
        total = List.foldl (\(_,b) c -> b+c) 0 converted_times
        workTimes = { times=converted_times, total=total }
    in
        { model | text=newText, workTimes=workTimes }

maybe_valid_time_entry line =
  let
    splitResult = Regex.split (Regex.AtMost 1) (Regex.regex " ") line
  in
    maybe_two splitResult
     `Maybe.andThen` valid_time_length
     `Maybe.andThen` numeric_min_hours
     `Maybe.andThen` valid_time_values

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

to_worked_times x =
  x
    |> zip_self
    |> List.map to_minutes
    |> List.foldl time_collect Dict.empty
    |> Dict.toList

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

store text nonWorkItems =
    setStorage { text=text, nonWorkItems=nonWorkItems }


-- VIEW

view : Model -> Html Msg
view model =
  div [ container_style ]
    [ div [ item_style ] [ make_input model.text ]
    , div [ item_style ] [ list_times model.workTimes ]
    --, p [ ] [ span [ class "icon-coffee" ] [ ], text "coffee" ]
    --, p [ ] [ span [ class "icon-briefcase" ] [ ], text "briefcase" ]
    ]
     
list_times { times, total } =
  let
    --format = \attr x -> li [(style attr), onClick (NonWork (fst x)) ] [ x |> format_time |> text]
    format = \attr x -> li [(style attr)] [ x |> format_time |> text]
    formatted_times = List.map (format []) times
    total_time = format [("color", "DarkGray")] ("Total", total)
    list_items = List.append formatted_times [total_time]
  in
  ul [] list_items

format_time x =
  let
    (name, total_mins) = x
    hrs = total_mins // 60
    mins = total_mins % 60
    string_hrs = toString hrs
    string_mins = toString mins
  in
    string_hrs ++ "h " ++ string_mins ++ "m - " ++ name

make_input text =
  textarea
    [ placeholder "0900 Example task"
    , value text
    , onInput Change
    , textarea_style
    ]
    []

container_style =
  style
    [ ("display", "flex")
    , ("height","100%")
    ]

item_style : Attribute msg
item_style =
  style
    [ ("flex-grow", "1")
    , ("flex-shrink", "1")
    , ("flex-basis", "150px")
    , ("height","100%")
    ]

textarea_style : Attribute msg
textarea_style =
  style
    [ ("resize", "none")
    , ("height","100%")
    , ("width", "100%")
    ]
