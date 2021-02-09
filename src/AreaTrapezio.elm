module AreaTrapezio exposing (main)
import Auxiliar exposing (calc3, basicTrait3)
import Browser exposing (sandbox)
import Html exposing (Html, div, text, h5, input, span)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)

-- Types
type alias Model = {i1 : String, i2 : String, i3 : String}
type Msg = Input1 String | Input2 String | Input3 String

-- Variables
calc = \n m p -> ((n + m)*p)/2
trait = basicTrait3
toText =
  {
    title = "Calcular Área do Trapézio",
    box1 = "Base",
    box2 = "Base",
    box3 = "Altura"
  }

-- Sandbox
main =
  sandbox
  {
    init = Model "" "" "",
    update = update,
    view = view
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input1 n ->
      {model | i1 = n}
    Input2 n ->
      {model | i2 = n}
    Input3 n ->
      {model | i3 = n}
  
view : Model -> Html Msg
view model =
  div [ class "card col-md-4 col-sm-12 bg-dark" ]
    [ div [ class "card-body bg-dark text-light" ]
      [ h5 [ class "card-title" ] [ text toText.title ],
        div [] [ div [ class "row" ]
          [ div [ class "col-md-4" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box1, type_ "number", onInput Input1 ] [] ],
            div [ class "col-md-4" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box2, type_ "number", onInput Input2 ] [] ],
            div [ class "col-md-4" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box3, type_ "number", onInput Input3 ] [] ] ],
            div [ class "row" ] [ div [ class "col-md-12" ] [ span [ class "h6" ] [ text "Resultado: " ], text <| calc3 calc trait model.i1 model.i2 model.i3 ] ] ] ] ]
