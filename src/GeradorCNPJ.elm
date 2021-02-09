module GeradorCNPJ exposing (main)
import Auxiliar exposing (charToInt, cnpjc, digv, intToChar)
import Browser exposing (element)
import Html exposing (Html, div, text, h5, span, button)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Random

-- Types
type alias Model = {o : String}
type Msg = Input | Calc Int | Output Int

-- Function
gerarCNPJ : String -> String
gerarCNPJ gen =
  let
    cnpjt = String.toList gen
    cnpjl = 
      if (List.length cnpjt) == 12 then
        List.map charToInt cnpjt
      else 
        List.map charToInt <| '0'::cnpjt
    pdig = digv <| cnpjc cnpjl
    sdig = digv <| cnpjc (List.append cnpjl [Just pdig])
    cnpj = List.append (List.append cnpjl [Just pdig]) [Just sdig]
    trait = List.map intToChar cnpj
  in
    case trait of
      a::b::c::d::e::f::g::h::i::j::k::l::m::n::xs ->
        a::b::'.'::c::d::e::'.'::f::g::h::'/'::i::j::k::l::'-'::m::n::xs
        |> String.fromList
      _ -> ""

-- Variables
toText =
  {
    title = "Gerador de CPF",
    btn = "Gerar"
  }

-- Element
main =
  element
    {
      init = init,
      update = update,
      subscriptions = \_ -> Sub.none,
      view = view
    }

init : () -> ( Model, Cmd Msg )
init _ = ( Model "", Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Input ->
      (model,Random.generate Calc <| Random.int 100000 999999)
    Calc n ->
      (Model (String.fromInt n),Random.generate Output <| Random.int 10000 999999)
    Output s ->
      (Model (model.o ++ (String.fromInt s)), Cmd.none)

view : Model -> Html Msg
view model =
  div [ class "card col-md-4 col-sm-12 bg-dark" ]
    [ div [ class "card-body bg-dark text-light" ]
      [ h5 [ class "card-title" ] [ text toText.title ],
        div [] [ div [ class "row" ]
          [ div [] [ button [ class "btn btn-outline-light", onClick Input, style "margin-right" "10px" ] [ text toText.btn ],
            span [class "h6"] [text "Resultado: "] , text <| gerarCNPJ model.o ] ] ] ] ]
