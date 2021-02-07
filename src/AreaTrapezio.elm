module AreaTrapezio exposing (..)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

areaTrapezio : String -> String -> String -> String
areaTrapezio bM bm h =
  case (String.toFloat bM, String.toFloat bm, String.toFloat h) of
    (Just n, Just m, Just p) ->
      if n > 0 && m > 0 && p > 0 then String.fromFloat (((n + m)*p)/2) ++ " u²"
      else "Indeterminado"
    _ -> "Indeterminado"

-- MAIN 
-- O programa principal é uma função chamada sandbox em que você passa um Record de ELM contendo três elementos: init (que é o modelo incial), update (função de atualização de valores do modelo) e view (função para visualização no Browser)
type alias Model = {i1 : String, i2 : String, i3 : String}
type Msg = Input1 String | Input2 String | Input3 String

main =
  Browser.sandbox
  {
    init = Model "" "" "",
    update = update,
    view = view
  }

-- UPDATE
-- Agora que temos um modelo, precisamos definir como ele muda com o tempo. É uma prática interessante sempre iniciar a seção de UPDATE do código, definindo um conjunto de mensagens que serão recebidas da interface do usuário: mensagem de incremento ou de decremento, neste exemplo.
update : Msg -> Model -> Model
update msg model =
  case msg of
    Input1 n -> {model | i1 = n}
    Input2 m -> {model | i2 = m}
    Input3 p -> {model | i3 = p}
  
-- VIEW
-- Neste trecho, ocorre a visualização do modelo para o usuário em HTML. A biblioteca html dá acesso total ao HTML5 como funções normais do Elm. A função view está produzindo um valor Html Msg. Os atributos onClick estão definidos para dar valores de Incremento e Decremento; eles serão alimentados diretamente na função de atualização, levando todo o aplicativo adiante.
view : Model -> Html Msg
view model =
  let
    toText = {output = areaTrapezio model.i1 model.i2 model.i3, title = "Calcular Área do Trapézio", box1 = "Base maior", box2 = "Base menor", box3 = "Altura"}
  in
    div [ class "card col-md-4 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
            [ h5 [ class "card-title" ] [ text toText.title ]
            , div []
                [ div [ class "row" ]
                    [ div [ class "col-md-4" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box1, type_ "number", onInput Input1 ] [] ]
                    , div [ class "col-md-4" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box2, type_ "number", onInput Input2 ] [] ]
                    , div [ class "col-md-4" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box3, type_ "number", onInput Input3 ] [] ]
                    ]
                , div [ class "row" ] [ div [ class "col-md-12" ] [ span [ class "h6" ] [ text "Resultado: " ], text toText.output ] ]
                ]
            ]
        ]