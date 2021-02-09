module AreaQuadrado exposing (main)
import Auxiliar exposing (calc1, basicTrait)
import Browser exposing (sandbox)
import Html exposing (Html, div, text, h5, input, span)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)

-- Types
type alias Model = {i1 : String}
type Msg =  Input String

-- Variables
calc = \n -> n^2
trait = basicTrait
toText =
  {
    title = "Calcular Área do Quadrado",
    box = "Lado"
  }

-- Sandbox
main =
  sandbox
  {
    init = Model "",
    update = update,
    view = view
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input n ->
      {model | i1 = n}
  
view : Model -> Html Msg
view model =
  div [ class "card col-md-4 col-sm-12 bg-dark" ]
    [ div [ class "card-body bg-dark text-light" ]
      [ h5 [ class "card-title" ] [ text toText.title ],
        div [] [ div [ class "row" ]
          [ div [ class "col-md-12" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box, type_ "number", onInput Input ] [] ] ],
            div [ class "row" ] [ div [ class "col-md-12" ] [ span [ class "h6" ] [ text "Resultado: " ], text <| calc1 calc trait model.i1 ] ] ] ] ]
