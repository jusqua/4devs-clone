module GeradorCPF exposing (..)
import Auxiliar exposing (digv, cpfc, charToInt, intToChar)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Browser

gerarCPF : Int -> String
gerarCPF gen =
  let
    cpfl = String.toList <| String.fromInt gen
    cpfl2 = 
      if (List.length cpfl) == 9 then
        List.map charToInt cpfl
      else 
        List.map charToInt <| '0'::cpfl
    pdig = digv <| cpfc cpfl2 10
    sdig = digv <| cpfc (List.append cpfl2 [Just pdig]) 11
    cpf = List.append (List.append cpfl2 [Just pdig]) [Just sdig]
    trait = List.map intToChar cpf
  in
    case trait of
      a::b::c::d::e::f::g::h::i::j::k::xs ->
        a::b::c::'.'::d::e::f::'.'::g::h::i::'-'::j::k::xs
        |> String.fromList
      _ -> ""


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
type alias Model = {genCPF : Int}

init : () -> (Model, Cmd Msg)
init _ = (Model 0, Cmd.none)

-- UPDATE
type Msg = Gen | CPF Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Gen ->
      (model,Random.generate CPF <| Random.int 10000000 999999999)
    CPF n ->
      (Model n,Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = 
  div []
    [ 
      button [onClick Gen, style "margin-right" "10px"] [text "Gerar"],
      text <| gerarCPF model.genCPF
    ]
