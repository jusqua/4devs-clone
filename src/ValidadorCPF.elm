module ValidadorCPF exposing (main)
import Browser exposing (sandbox)
import Auxiliar exposing (cpfc, cpfncnpjTrait, digg, digv)
import Html exposing (Html, text, input, button, div, h5, span)
import Html.Attributes exposing (placeholder, value, class)
import Html.Events exposing (onClick, onInput)

-- Types
type alias Model = {i : String, o : String}
type Msg = Input String | Output

-- Function
validarCPF : String -> String
validarCPF input =
  let
    cpf = cpfncnpjTrait input True
  in
    if List.length cpf == 11 then
      let
        odig = digg cpf
        cpft = List.take 9 cpf
        pdig = digv <| cpfc cpft 10
        sdig = digv <| cpfc (List.append cpft <| [ Just pdig ]) 11
      in
        if pdig == Tuple.first odig && sdig == Tuple.second odig then
          "Válido"
        else
          "Inválido"
    else
      "Indefinido"

-- Variables
toText =
  {
    title = "Validador de CPF",
    box = "CPF",
    btn = "Validar"
  }

-- Sandbox
main =
  sandbox
    { 
      init = Model "" "",
      update = update,
      view = view
    }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input n ->
      { model | i = n }
    Output ->
      { model | o = validarCPF model.i }

view : Model -> Html Msg
view model =
  div [ class "card col-md-4 col-sm-12 bg-dark" ]
    [ div [ class "card-body bg-dark text-light" ]
      [ h5 [ class "card-title" ] [ text toText.title ],
        div [] [ div [ class "row" ]
          [ input [class "form-control bg-dark text-light", placeholder toText.box, value model.i, onInput Input ] [],
            button [class "btn btn-outline-light mt-2", onClick Output] [ text toText.btn ] ],
            div [ class "row" ] [span [ class "h6" ] [ text "Resultado: ", text model.o ] ] ] ] ]
