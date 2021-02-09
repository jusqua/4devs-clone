module ValidadorCNPJ exposing (main)
import Browser exposing (sandbox)
import Auxiliar exposing (cnpjc, cpfncnpjTrait, digg, digv)
import Html exposing (Html, text, input, button, div, h5, span)
import Html.Attributes exposing (placeholder, value, class)
import Html.Events exposing (onClick, onInput)

-- Types
type alias Model = {i : String, o : String}
type Msg = Input String | Output

-- Function
validarCNPJ : String -> String
validarCNPJ input =
  let
    cnpj = cpfncnpjTrait input False
  in
    if List.length cnpj == 14 then
      let
        odig = digg cnpj
        cnpjt = List.take 12 cnpj
        pdig = digv <| cnpjc cnpjt
        sdig = digv <| cnpjc <| List.append cnpjt [ Just pdig ]
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
    title = "Validador de CNPJ",
    box = "CNPJ",
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
      { model | o = validarCNPJ model.i }

view : Model -> Html Msg
view model =
  div [ class "card col-md-4 col-sm-12 bg-dark" ]
    [ div [ class "card-body bg-dark text-light" ]
      [ h5 [ class "card-title" ] [ text toText.title ],
        div [] [ div [ class "row" ]
          [ input [class "form-control bg-dark text-light", placeholder toText.box, value model.i, onInput Input ] [],
            button [class "btn btn-outline-light mt-2", onClick Output] [ text toText.btn ] ],
            div [ class "row" ] [span [ class "h6" ] [ text "Resultado: ", text model.o ] ] ] ] ]
