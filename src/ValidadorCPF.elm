module ValidadorCPF exposing (..)

import Auxiliar exposing (cpfc, cpfncnpjTrait, digg, digv)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


validarCPF : String -> String
validarCPF input =
    let
        cpf =
            cpfncnpjTrait input True
    in
    if List.length cpf == 11 then
        let
            odig =
                digg cpf

            cpft =
                List.take 9 cpf

            pdig =
                digv <| cpfc cpft 10

            sdig =
                digv <| cpfc (List.append cpft <| [ Just pdig ]) 11
        in
        if pdig == Tuple.first odig && sdig == Tuple.second odig then
            "Válido"

        else
            "Inválido"

    else
        "Indefinido"



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { inputCPF : String, outputCPF : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "", Cmd.none )



-- UPDATE


type Msg
    = CPF String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CPF cpf ->
            ( { model | inputCPF = cpf }, Cmd.none )

        Submit ->
            ( { model | outputCPF = validarCPF model.inputCPF }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "card col-md-4 col-sm-12 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
            [ h5 [ class "card-title" ] [ text "Validador de CPF" ]
            , div [ class "row" ]
                [ input [ class "form-control bg-dark text-light", type_ "text", placeholder "CPF", value model.inputCPF, onInput CPF ] []
                , button [ class "btn btn-outline-light mt-2", onClick Submit ] [ text "Validar" ]
                ]
            , span [ class "h6" ] [ text "Resultado: ", text model.outputCPF ]
            ]
        ]
