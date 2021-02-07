module AreaTriangulo exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


areaTriangulo : String -> String -> String
areaTriangulo b h =
    case ( String.toFloat b, String.toFloat h ) of
        ( Just n, Just m ) ->
            if n > 0 && m > 0 then
                String.fromFloat ((n * m) / 2) ++ " u²"

            else
                "Indeterminado"

        _ ->
            "Indeterminado"



-- MAIN
-- O programa principal é uma função chamada sandbox em que você passa um Record de ELM contendo três elementos: init (que é o modelo incial), update (função de atualização de valores do modelo) e view (função para visualização no Browser)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL
-- Nesta aplicação, o que precisa ser gerenciado é o contador em si, pois é ele que terá seu valor alterado a depender das interações do usuário. Portanto, esse valor inteiro representa o MODEL e inicia com valor 0 (zero).


type alias Model =
    { base : String, altura : String }


init : Model
init =
    Model "" ""



-- UPDATE
-- Agora que temos um modelo, precisamos definir como ele muda com o tempo. É uma prática interessante sempre iniciar a seção de UPDATE do código, definindo um conjunto de mensagens que serão recebidas da interface do usuário: mensagem de incremento ou de decremento, neste exemplo.


type Msg
    = Calcular
    | Input1 String
    | Input2 String



-- A partir daí, a função de atualização apenas descreve o que fazer quando se recebe uma dessas mensagens. Se receber uma mensagem de incremento, incrementa-se o modelo; se receber uma mensagem de decremento, diminui-se o modelo.


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input1 n ->
            { model | base = n }

        Input2 m ->
            { model | altura = m }

        _ ->
            model



-- VIEW
-- Neste trecho, ocorre a visualização do modelo para o usuário em HTML. A biblioteca html dá acesso total ao HTML5 como funções normais do Elm. A função view está produzindo um valor Html Msg. Os atributos onClick estão definidos para dar valores de Incremento e Decremento; eles serão alimentados diretamente na função de atualização, levando todo o aplicativo adiante.


view : Model -> Html Msg
view model =
    div [ class "card col-md-4 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
            [ h5 [ class "card-title" ] [ text "Calcular Área do Triângulo" ]
            , div []
                [ div [ class "row" ]
                    [ div [ class "col-md-6" ] [ input [ class "form-control bg-dark text-light", placeholder "Base", type_ "number", onInput Input1 ] [] ]
                    , div [ class "col-md-6" ] [ input [ class "form-control bg-dark text-light", placeholder "Altura", type_ "number", onInput Input2 ] [] ]
                    ]
                , div [ class "row" ] [ div [ class "col-md-6" ] [ span [ class "h6" ] [ text "Resultado: " ], text <| areaTriangulo model.base model.altura ] ]
                ]
            ]
        ]
