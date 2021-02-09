module GeradorCPF exposing (main)
import Auxiliar exposing (charToInt, cpfc, digv, intToChar)
import Browser exposing (element)
import Html exposing (Html, div, text, h5, span, button)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Random

-- Types
type alias Model = {o : String}
type Msg = Input | Output Int

-- Function
gerarCPF : String -> String
gerarCPF gen =
  let
    cpfl = String.toList gen
    cpfl2 =
      if List.length cpfl == 9 then
        List.map charToInt cpfl
      else
        List.map charToInt <| '0' :: cpfl
    pdig = digv <| cpfc cpfl2 10
    sdig = digv <| cpfc (List.append cpfl2 [ Just pdig ]) 11
    cpf = List.append (List.append cpfl2 [ Just pdig ]) [ Just sdig ]
    trait = List.map intToChar cpf
  in
    case trait of
      a::b::c::d::e::f::g::h::i::j::k::xs ->
        a::b::c::'.'::d::e::f::'.'::g::h::i::'-'::j::k::xs
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
      (model, Random.generate Output <| Random.int 10000000 999999999)
    Output n ->
      (Model (String.fromInt n), Cmd.none)

view : Model -> Html Msg
view model =
  div [ class "card col-md-4 col-sm-12 bg-dark" ]
    [ div [ class "card-body bg-dark text-light" ]
      [ h5 [ class "card-title" ] [ text toText.title ],
        div [] [ div [ class "row" ]
          [ div [] [ button [ class "btn btn-outline-light", onClick Input, style "margin-right" "10px" ] [ text toText.btn ],
            span [class "h6"] [text "Resultado: "] , text <| gerarCPF model.o ] ] ] ] ]
