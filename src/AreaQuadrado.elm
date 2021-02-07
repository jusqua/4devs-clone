module AreaQuadrado exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


areaQuadrado : String -> String
areaQuadrado l =
    case String.toFloat l of
        Just n ->
            if n > 0.0 then
                String.fromFloat (n ^ 2) ++ " u²"

            else
                "Indeterminado"

        Nothing ->
            "Indeterminado"



-- MAIN
-- O programa principal é uma função chamada sandbox em que você passa um Record de ELM contendo três elementos: init (que é o modelo incial), update (função de atualização de valores do modelo) e view (função para visualização no Browser)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    String


init : Model
init =
    "Indeterminado"


type Msg
    = Calcular
    | Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input newContent ->
            areaQuadrado newContent

        _ ->
            model


view : Model -> Html Msg
view model =
    div [ class "card col-md-4 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
            [ h5 [ class "card-title" ] [ text "Calcular Área do Quadrado" ]
            , div []
                [ div [ class "row" ]
                    [ div [ class "col-md-12" ] [ input [ class "form-control bg-dark text-light", placeholder "Lado", type_ "number", onInput Input, style "margin-right" "10px" ] [] ] ]
                , div [ class "row" ] [ div [ class "col-md-6" ] [ span [ class "h6" ] [ text "Resultado: " ], text model ] ]
                ]
            ]
        ]
