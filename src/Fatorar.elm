module Fatorar exposing (main)
import Browser exposing (sandbox)
import Html exposing (Html, div, text, h5, input, span)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)

-- Types
type alias Model = {i1 : String}
type Msg =  Input String

-- Variables
toText =
  {
    title = "Fatoração Numérica",
    box = "Número"
  }

-- Function
fatorar : String -> String
fatorar f =
  let
    aux : Int -> Int -> String
    aux num fct =
      if num == fct || num == 1 then String.fromInt num
      else if (modBy fct num) == 0 then (String.fromInt fct) ++ "." ++ aux (num//fct) fct
      else aux num (fct+1)
  in
  case String.toInt f of
  Just n ->
    if n == 0 then "0"
    else if n > 0 then aux n 2
    else "-" ++ (aux (-n) 2)
  Nothing -> "Indeterminado"

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
            div [ class "row" ] [ div [ class "col-md-12" ] [ span [ class "h6" ] [ text "Resultado: " ], text <| fatorar model.i1 ] ] ] ] ]
