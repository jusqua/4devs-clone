module ValidadorCPF exposing (..)
import Auxiliar exposing (cpfc, digv, cpfncnpjTrait, digg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Browser

validarCPF : String -> String
validarCPF input = 
  let
    cpf = cpfncnpjTrait input
  in
    if (List.length cpf) == 11 then
      let
        odig = digg cpf
        cpft = List.take 9 cpf
        pdig = digv <| cpfc cpft 10
        sdig = digv <| cpfc (List.append cpft <| [Just pdig]) 11
      in
        if (pdig == (Tuple.first odig) && sdig == (Tuple.second odig)) then 
          "Válido"
        else 
          "Inválido"
    else 
      "Indefinido"

-- MAIN
main =
  Browser.element
  {
    init = init,
    update = update,
    subscriptions = \model -> Sub.none,
    view = view
  }

-- MODEL
type alias Model = {inputCPF : String, outputCPF : String}

init : () -> (Model, Cmd Msg)
init _ = (Model "" "", Cmd.none)

-- UPDATE
type Msg = CPF String | Submit

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CPF cpf ->
      ({model | inputCPF = cpf},Cmd.none)
    Submit ->
      ({model | outputCPF = validarCPF model.inputCPF},Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = 
  div []
  [
    input [type_ "text", placeholder "CPF", value model.inputCPF, onInput CPF] [],
    button [onClick Submit, style "margin-right" "10px"] [text "Validar"],
    text model.outputCPF
  ]
