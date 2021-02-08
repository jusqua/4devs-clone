module GeradorCNPJ exposing (..)
import Auxiliar exposing (digv, cnpjc, charToInt, intToChar)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Browser

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
type alias Model = {genCNPJ : String}

init : () -> (Model, Cmd Msg)
init _ = (Model "", Cmd.none)

-- UPDATE
type Msg = Gen | Calc Int | CNPJ Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Gen ->
      (model,Random.generate Calc <| Random.int 100000 999999)
    Calc n ->
      (Model (String.fromInt n),Random.generate CNPJ <| Random.int 10000 999999)
    CNPJ s ->
      (Model (model.genCNPJ ++ (String.fromInt s)), Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = 
   div
        [ class "card col-md-4 col-sm-12 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
            [ h5 [ class "card-title" ] [ text "Gerador de CNPJ" ]
            , div []
                [ div [ class "row" ]
                    [ div []
                        [ button [ class "btn btn-outline-light", onClick Gen, style "margin-right" "10px" ] [ text "Gerar" ],
                        span [class "h6"] [text "Resultado: "] , text <| gerarCNPJ model.genCNPJ
                        ]
                    ]
                ]
            ]
        ]
