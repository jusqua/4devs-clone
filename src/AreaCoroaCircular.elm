module AreaCoroaCircular exposing (main)
import Auxiliar exposing (calc2)
import Browser exposing (sandbox)
import Html exposing (Html, div, text, h5, input, span)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)

-- Types
type alias Model = {i1 : String, i2 : String}
type Msg = Input1 String | Input2 String

-- Variables
calc = \n m -> pi*(n^2 - m^2)
trait = \n m -> (n >= 0 && m >= 0 && n >= m)
toText =
  {
    title = "Calcular Área da Coroa Circular",
    box1 = "Raio Maior/Externo",
    box2 = "Raio Menor/Interno"
  }

-- Sandbox
main =
  sandbox
  {
    init = Model "" "",
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
  
view : Model -> Html Msg
view model =
  div [ class "card col-md-4 col-sm-12 bg-dark" ]
    [ div [ class "card-body bg-dark text-light" ]
      [ h5 [ class "card-title" ] [ text toText.title ],
        div [] [ div [ class "row" ]
          [ div [ class "col-md-6" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box1, type_ "number", onInput Input1 ] [] ],
            div [ class "col-md-6" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box2, type_ "number", onInput Input2 ] [] ] ],
            div [ class "row" ] [ div [ class "col-md-12" ] [ span [ class "h6" ] [ text "Resultado: " ], text <| calc2 calc trait model.i1 model.i2 ] ] ] ] ]
