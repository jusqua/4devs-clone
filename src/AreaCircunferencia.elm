module AreaCircunferencia exposing (..)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

areaCircunferencia : String -> String
areaCircunferencia r =
  case String.toFloat r of
  Just n ->
    if n>0.0 then String.fromFloat(pi * n^2) ++ " u²"
    else "Indeterminado"
  Nothing -> "Indeterminado"

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = String

init : Model 
init = "Indeterminado"

type Msg = Calcular | Input String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input newContent-> areaCircunferencia newContent
    _ -> model
  
view : Model -> Html Msg
view model =
    div [ class "card col-md-4 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
            [ h5 [ class "card-title" ] [ text "Calcular Área da Circunferência" ]
            , div []
                [ div [ class "row" ]
                    [ div [ class "col-md-12" ] [ input [ class "form-control bg-dark text-light", placeholder "Raio", type_ "number", onInput Input, style "margin-right" "10px" ] [] ] ]
                , div [ class "row" ] [ div [ class "col-md-12" ] [ span [ class "h6" ] [ text "Resultado: " ], text model ] ]
                ]
            ]
        ]

    