module Fatorar exposing (..)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

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
    else "-(" ++ (aux (-n) 2) ++ ")"
  Nothing -> "Indeterminado"

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
    toText = {output = fatorar model.i1, title = "Fatoração Numérica", box1 = "Número"}
  in
    div 
      [ class "card col-md-4 col-sm-12 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
          [ h5 [ class "card-title" ] [ text toText.title ],
            div []
              [ div [ class "row" ]
                [ 
                  div [ class "col-md-12" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box1, type_ "number", onInput Input, style "margin-right" "10px" ] [] ] ],
                  div [ class "row" ] [ div [ class "col-md-12" ] [ span [ class "h6" ] [ text "Resultado: " ], text toText.output ]
                ]
              ]
          ]
        ]
