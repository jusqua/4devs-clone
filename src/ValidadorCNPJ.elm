module ValidadorCNPJ exposing (..)

import Auxiliar exposing (cnpjc, cpfncnpjTrait, digg, digv)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


validarCNPJ : String -> String
validarCNPJ input =
    let
        cnpj =
            cpfncnpjTrait input
    in
    if List.length cnpj == 14 then
        let
            odig =
                digg cnpj

            cnpjt =
                List.take 12 cnpj

            pdig =
                digv <| cnpjc cnpjt

            sdig =
                digv <| cnpjc <| List.append cnpjt [ Just pdig ]
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
    { inputCNPJ : String, outputCNPJ : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "", Cmd.none )



-- UPDATE


type Msg
    = CNPJ String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CNPJ cnpj ->
            ( { model | inputCNPJ = cnpj }, Cmd.none )

        Submit ->
            ( { model | outputCNPJ = validarCNPJ model.inputCNPJ }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
   div
        [ class "card col-md-4 col-sm-12 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
            [ h5 [ class "card-title" ] [ text "Validador de CNPJ" ]
            , div []
                [ div [ class "row" ]
                    [ input [class "form-control bg-dark text-light", type_ "text", placeholder "CNPJ", value model.inputCNPJ, onInput CNPJ ] []
                    , button [class "btn btn-outline-light mt-2", onClick Submit] [ text "Validar" ]
                    ],
                    div[class "row"] [span [class "h6"][text "Resultado: ", text model.outputCNPJ]]
                ]
            ]
        ]
