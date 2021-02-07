module ContadorString exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias StringValues =
    { c : Int
    , cse : Int
    , e : Int
    , l : Int
    , v : Int
    , con : Int
    , n : Int
    }


isVowel : Char -> Bool
isVowel ch =
    case Char.toLocaleLower ch of
        'a' ->
            True

        'e' ->
            True

        'i' ->
            True

        'o' ->
            True

        'u' ->
            True

        _ ->
            False


isConsonant : Char -> Bool
isConsonant ch =
    if Char.isAlpha ch then
        if isVowel ch then
            False

        else
            True

    else
        False


contadorString : String -> StringValues
contadorString str =
    let
        lch =
            String.toList str
    in
    if String.length str > 0 then
        { c = String.length str
        , cse = List.length <| List.filter (\ch -> ch /= ' ') lch
        , e = List.length <| List.filter (\ch -> ch == ' ') lch
        , l = List.length <| List.filter (\ch -> ch == '\n') lch
        , v = List.length <| List.filter isVowel lch
        , con = List.length <| List.filter isConsonant lch
        , n = List.length <| List.filter Char.isDigit lch
        }

    else
        { c = 0
        , cse = 0
        , e = 0
        , l = 0
        , v = 0
        , con = 0
        , n = 0
        }



-- MAIN
-- O programa principal é uma função chamada sandbox em que você passa um Record de ELM contendo três elementos: init (que é o modelo incial), update (função de atualização de valores do modelo) e view (função para visualização no Browser)


type alias Model =
    { i1 : String }


type Msg
    = Input String


main =
    Browser.sandbox
        { init = Model ""
        , update = update
        , view = view
        }



-- UPDATE
-- Agora que temos um modelo, precisamos definir como ele muda com o tempo. É uma prática interessante sempre iniciar a seção de UPDATE do código, definindo um conjunto de mensagens que serão recebidas da interface do usuário: mensagem de incremento ou de decremento, neste exemplo.


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input n ->
            { model | i1 = n }



-- VIEW
-- Neste trecho, ocorre a visualização do modelo para o usuário em HTML. A biblioteca html dá acesso total ao HTML5 como funções normais do Elm. A função view está produzindo um valor Html Msg. Os atributos onClick estão definidos para dar valores de Incremento e Decremento; eles serão alimentados diretamente na função de atualização, levando todo o aplicativo adiante.


view : Model -> Html Msg
view model =
    let
        toText =
            { output = contadorString model.i1, title = "Contador de Texto", box1 = "Texto" }
    in
    div
        [ class "card col-md-4 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
            [ h5 [ class "card-title" ] [ text toText.title ]
            , div []
                [ input [ class "form-control bg-dark text-light", placeholder toText.box1, onInput Input, style "margin-right" "10px" ] [] ]
            ]
        , div [ class "row", style "margin-left" "5px" ] [ div [ class "col-md-4" ] [ span [ class "h6 text-light" ] [ text <| "Caractéres: " ++ String.fromInt toText.output.c ] ], div [ class "col-md-8" ] [ span [ class "h6 text-light" ] [ text <| "Caractéres sem espaço: " ++ String.fromInt toText.output.cse ] ] ]
        , div [ class "row", style "margin-left" "5px" ] [ div [ class "col-md-4" ] [ span [ class "h6 text-light" ] [ text <| "Espaços: " ++ String.fromInt toText.output.e ] ], div [ class "col-md-8" ] [ span [ class "h6 text-light" ] [ text <| "Linhas: " ++ String.fromInt toText.output.l ] ] ]
        , div [ class "row mb-3", style "margin-left" "5px" ] [ div [ class "col-md-4" ] [ span [ class "h6 text-light" ] [ text <| "Vogais: " ++ String.fromInt toText.output.v ] ], div [ class "col-md-5" ] [ span [ class "h6 text-light" ] [ text <| "Consoantes: " ++ String.fromInt toText.output.con ] ], div [ class "col-md-3" ] [ span [ class "h6 text-light" ] [ text <| "Números: " ++ String.fromInt toText.output.n ] ] ]
        ]
