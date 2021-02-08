module ReverterString exposing (..)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

-- MAIN 
-- O programa principal é uma função chamada sandbox em que você passa um Record de ELM contendo três elementos: init (que é o modelo incial), update (função de atualização de valores do modelo) e view (função para visualização no Browser)
type alias Model = {i1 : String}
type Msg = Input String

main =
  Browser.sandbox
  {
    init = Model "",
    update = update,
    view = view
  }

-- UPDATE
-- Agora que temos um modelo, precisamos definir como ele muda com o tempo. É uma prática interessante sempre iniciar a seção de UPDATE do código, definindo um conjunto de mensagens que serão recebidas da interface do usuário: mensagem de incremento ou de decremento, neste exemplo.
update : Msg -> Model -> Model
update msg model =
  case msg of
    Input n -> {model | i1 = n}
  
-- VIEW
-- Neste trecho, ocorre a visualização do modelo para o usuário em HTML. A biblioteca html dá acesso total ao HTML5 como funções normais do Elm. A função view está produzindo um valor Html Msg. Os atributos onClick estão definidos para dar valores de Incremento e Decremento; eles serão alimentados diretamente na função de atualização, levando todo o aplicativo adiante.
view : Model -> Html Msg
view model =
  let
    toText = {output = String.reverse model.i1, title = "Reverter Texto", box1 = "Texto"}
  in
    div 
      [ class "card col-md-4 col-sm-12 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
          [ h5 [ class "card-title" ] [ text toText.title ],
            div []
              [ div [ class "row" ]
                [ 
                  div [ class "col-md-12" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box1, type_ "text", onInput Input, style "margin-right" "10px" ] [] ] ],
                  div [ class "row" ] [ div [ class "col-md-12" ] [  span [ class "h6" ] [ text "Resultado: " ], span [ class "h6" ] [ text toText.output ] ]
                ]
              ]
          ]
        ]
