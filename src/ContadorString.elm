module ContadorString exposing (main)
import Auxiliar exposing (isVowel, isConsonant, isSpecialChar, getWords)
import Browser exposing (sandbox)
import Html exposing (Html, div, h5, text, textarea, span)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onInput)

-- Types
type alias StringValues = {c : Int, cse : Int, e : Int, l : Int, v : Int, con : Int, n : Int, es : Int, p : Int}
type alias Model = {i1 : String}
type Msg = Input String

-- Function
contadorString : String -> StringValues
contadorString str =
  let
    lch = String.toList str
    count : (Char -> Bool) -> List Char -> Int
    count f l = List.length <| List.filter f l
  in
    if String.length str > 0 then
      {
        c   = count (\ch -> ch /= '\n') lch,
        cse = count  (\ch -> ch /= ' ' && ch /= '\n') lch,
        e   = count  (\ch -> ch == ' ') lch,
        l   = (+) 1 <| count (\ch -> ch == '\n') lch,
        v   = count isVowel lch,
        con = count isConsonant lch,
        n   = count Char.isDigit lch,
        es  = count isSpecialChar lch,
        p   = getWords lch
      }
    else {c = 0, cse = 0, e = 0, l = 0, v = 0, con = 0, n = 0, es = 0, p = 0}

-- Sandbox
main =
  sandbox
    {
      init = Model "",
      update = update,
      view = view
    }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input n ->
      { model | i1 = n }

view : Model -> Html Msg
view model =
  let
      output = contadorString model.i1
  in
    div [ class "card col-md-4 col-sm-12 bg-dark" ]
      [ div [ class "card-body bg-dark text-light" ]
        [ h5 [ class "card-title" ] [ text "Contador de Strings" ],
          div [] [ textarea [ class "form-control bg-dark text-light", onInput Input, style "resize" "none" ] [] ] ],
          div [ class "row mb-3", style "margin-left" "5px" ] [ div [ class "col-md-4 col-sm-12" ] [ span [ class "h6 text-light" ] [ text <| "Caractéres: " ++ String.fromInt output.c ] ], div [ class "col-md-5" ] [ span [ class "h6 text-light" ] [ text <| "Caractéres sem espaço: " ++ String.fromInt output.cse ] ], div [ class "col-md-3" ] [ span [ class "h6 text-light" ] [ text <| "Palavras: " ++ String.fromInt output.p ] ] ],
          div [ class "row mb-3", style "margin-left" "5px" ] [ div [ class "col-md-4 col-sm-12" ] [ span [ class "h6 text-light" ] [ text <| "Espaços: " ++ String.fromInt output.e ] ], div [ class "col-md-5" ] [ span [ class "h6 text-light" ] [ text <| "Linhas: " ++ String.fromInt output.l ] ], div [ class "col-md-3" ] [ span [ class "h6 text-light" ] [ text <| "Especial: " ++ String.fromInt output.es ] ] ],
          div [ class "row mb-3", style "margin-left" "5px" ] [ div [ class "col-md-4 col-sm-12" ] [ span [ class "h6 text-light" ] [ text <| "Vogais: " ++ String.fromInt output.v ] ], div [ class "col-md-5" ] [ span [ class "h6 text-light" ] [ text <| "Consoantes: " ++ String.fromInt output.con ] ], div [ class "col-md-3" ] [ span [ class "h6 text-light" ] [ text <| "Números: " ++ String.fromInt output.n ] ] ] ]
