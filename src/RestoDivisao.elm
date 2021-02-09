module RestoDivisao exposing (main)
import Browser exposing (sandbox)
import Html exposing (Html, div, text, h5, input, span)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)

-- Types
type alias Model = {i1 : String, i2 : String}
type Msg = Input1 String | Input2 String

-- Variables
toText =
  {
    title = "Resto da Divisão",
    box1 = "Numerador",
    box2 = "Denominador"
  }

-- Function
restoDivisao : String -> String -> String
restoDivisao a b =
  case (String.toInt a, String.toInt b) of
    (Just n, Just m) ->
      if n >= 0 && m > 0 && n >= m then String.fromInt (n-(n//m)*m)
      else "Indeterminado"
    _ -> "Indeterminado"

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
            div [ class "row" ] [ div [ class "col-md-12" ] [ span [ class "h6" ] [ text "Resultado: " ], text <| restoDivisao model.i1 model.i2 ] ] ] ] ]
