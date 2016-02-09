import Html exposing (Html, Attribute, text, toElement, div, textarea, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String

type alias Model = { text:String, times:List String }

main =
  StartApp.start { model = initial_model, view = view, update = update }


initial_model = 
  { text="", times=[] }



update : String -> Model -> Model
update newStr oldModel =
  let
    items = String.split "\n" newStr
  in
    {text=newStr, times=items}



view : Address String -> Model -> Html
view address model =
  div [ container_style ]
    [ div [ item_style ] [ make_input address model.text ]
    , div [ item_style ] [ list model.times ]
    ]
     
list n =
  ul [] (List.map (\x -> li [] [text x]) n)

make_input address text =
  textarea
    [ placeholder "0900 Example task"
    , value text
    , on "input" targetValue (Signal.message address)
    , textarea_style
    ]
    []

container_style =
  style
    [ ("display", "flex")
    ]

item_style : Attribute
item_style =
  style
    [ ("flex-grow", "1")
    , ("flex-shrink", "1")
    , ("flex-basis", "150px")
    ]

textarea_style : Attribute
textarea_style =
  style
    [ ("resize", "none")
    , ("height","100%")
    , ("width", "100%")
    ]
