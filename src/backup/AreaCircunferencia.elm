module AreaCircunferencia exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    Float


init : Model
init =
    0


type Msg
    = Calcular
    | Input String



-- A partir daí, a função de atualização apenas descreve o que fazer quando se recebe uma dessas mensagens. Se receber uma mensagem de incremento, incrementa-se o modelo; se receber uma mensagem de decremento, diminui-se o modelo.


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input newContent ->
            pi * (Maybe.withDefault 0 (String.toFloat newContent) ^ 2)

        _ ->
            model


view : Model -> Html Msg
view model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-md-12" ] [ input [ class "form-control", placeholder "Tamanho do raio", type_ "number", onInput Input, style "margin-right" "10px" ] [] ]
            ]
        , div [ class "row" ] [ div [ class "col-md-12" ] [span[class "h6"][text "Resultado: "], text (String.fromFloat model) ] ]
        ]
