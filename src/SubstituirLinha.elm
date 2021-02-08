module SubstituirLinha exposing (..)
import Html exposing (textarea)
import Html.Attributes exposing (class)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

substituirLinha : String -> String -> String
substituirLinha text ntext =
  let
    aux : List Char -> List Char -> List Char
    aux ls ts =
      case ls of
        [] -> []
        x::xs -> if x == '\n' then List.append ts (aux xs ts) else x::(aux xs ts)
  in
    String.fromList <| aux (String.toList text) (String.toList ntext)
    
-- MAIN 
-- O programa principal é uma função chamada sandbox em que você passa um Record de ELM contendo três elementos: init (que é o modelo incial), update (função de atualização de valores do modelo) e view (função para visualização no Browser)
type alias Model = {i1 : String, i2 : String}
type Msg = Input1 String | Input2 String

main =
  Browser.sandbox
  {
    init = Model "" "",
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
  
-- VIEW
-- Neste trecho, ocorre a visualização do modelo para o usuário em HTML. A biblioteca html dá acesso total ao HTML5 como funções normais do Elm. A função view está produzindo um valor Html Msg. Os atributos onClick estão definidos para dar valores de Incremento e Decremento; eles serão alimentados diretamente na função de atualização, levando todo o aplicativo adiante.
view : Model -> Html Msg
view model =
  let
    toText = {output = substituirLinha model.i1 model.i2, title = "Substituir Quebra de Linha", box1 = "Texto Original", box2 = "Texto Substituto"}
  in
    div [ class "card col-md-4 col-sm-12 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
            [ h5 [ class "card-title" ] [ text toText.title ]
            , div []
                [ div [ class "row" ]
                    [ div [ class "col-md-12"] [textarea [class "form-control bg-dark text-light", placeholder toText.box1, onInput Input1, style "margin-right" "10px" ] [] ]]
                    , div [ class "col-md-12 mt-2" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box2, type_ "text", onInput Input2 ] [] ]
                    ]
                , div [ class "row" ] [ div [ class "col-md-12" ] [ span [ class "h6" ] [ text "Resultado: \n" ], text toText.output ] ]
                ]
            ]
