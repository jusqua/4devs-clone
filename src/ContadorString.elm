module ContadorString exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Dict exposing (intersect)

type alias StringValues =
    { c : Int
    , cse : Int
    , e : Int
    , l : Int
    , v : Int
    , con : Int
    , n : Int
    , es : Int
    , p : Int
    }

isVowel : Char -> Bool
isVowel ch =
  let
    chc = Char.toCode ch
    vowels = [65,69,73,79,85,97,101,105,111,117,129,192,193,194,195,196,197,198,200,201,202,203,204,205,206,207,210,211,212,213,214,216,217,218,219,220,224,225,226,227,228,229,230,232,233,234,235,236,237,238,239,242,243,244,245,246,248,249,250,251,252]
  in
    case List.filter (\c -> c == chc) vowels of
    [x] -> True
    _   -> False

isConsonant : Char -> Bool
isConsonant ch =
  let
    chc = Char.toCode ch
    sconsonants = [138,142,154,158,159,199,209,221,231,241,253,255]
  in
  if Char.isAlpha ch && not (isVowel ch) then True
  else
    case List.filter (\c -> c== chc) sconsonants of
    [x] -> True
    _   -> False
  
isSpecialChar : Char -> Bool
isSpecialChar ch =
  if (ch == ' ' || ch == '\n' || isVowel ch || isConsonant ch) then False
  else True

getWords : List Char -> Int
getWords lch =
  case lch of
    [] -> 0
    x::y::xs -> 
      if (x /= ' ' || x /= '\n') && (y == ' ' || y == '\n') then 1 + getWords (List.append [y] xs)
      else 0 + getWords (List.append [y] xs)
    [x] -> if x == ' ' || x == '\n' then 0 else 1

contadorString : String -> StringValues
contadorString str =
    let
        lch =
            String.toList str
    in
    if String.length str > 0 then
        { c = List.length <| List.filter (\ch -> ch /= '\n') lch
        , cse = List.length <| List.filter (\ch -> ch /= ' ' && ch /= '\n') lch
        , e = List.length <| List.filter (\ch -> ch == ' ') lch
        , l = List.length <| List.filter (\ch -> ch == '\n') lch
        , v = List.length <| List.filter isVowel lch
        , con = List.length <| List.filter isConsonant lch
        , n = List.length <| List.filter Char.isDigit lch
        , es = List.length <| List.filter isSpecialChar lch
        , p = getWords lch
        }

    else
        { c = 0
        , cse = 0
        , e = 0
        , l = 0
        , v = 0
        , con = 0
        , n = 0
        , es = 0
        , p = 0
        }



-- MAIN
-- O programa principal é uma função chamada sandbox em que você passa um Record de ELM contendo três elementos: init (que é o modelo incial), update (função de atualização de valores do modelo) e view (função para visualização no Browser)


type alias Model =
    { i1 : String }


type Msg
    = Input String


main =
    Browser.sandbox
        { init = Model ""
        , update = update
        , view = view
        }



-- UPDATE
-- Agora que temos um modelo, precisamos definir como ele muda com o tempo. É uma prática interessante sempre iniciar a seção de UPDATE do código, definindo um conjunto de mensagens que serão recebidas da interface do usuário: mensagem de incremento ou de decremento, neste exemplo.


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input n ->
            { model | i1 = n }



-- VIEW
-- Neste trecho, ocorre a visualização do modelo para o usuário em HTML. A biblioteca html dá acesso total ao HTML5 como funções normais do Elm. A função view está produzindo um valor Html Msg. Os atributos onClick estão definidos para dar valores de Incremento e Decremento; eles serão alimentados diretamente na função de atualização, levando todo o aplicativo adiante.


view : Model -> Html Msg
view model =
    let
        toText =
            { output = contadorString model.i1, title = "Contador de Texto", box1 = "Texto" }
    in
    div
        [ class "card col-md-4 col-sm-12 bg-dark" ]
        [ div [ class "card-body bg-dark text-light" ]
            [ h5 [ class "card-title" ] [ text toText.title ]
            , div []
                [ textarea [ class "form-control bg-dark text-light", placeholder toText.box1, onInput Input, style "margin-right" "10px" ] [] ]
            ]
        , div [ class "row mb-3", style "margin-left" "5px" ] [ div [ class "col-md-4 col-sm-12" ] [ span [ class "h6 text-light" ] [ text <| "Caractéres: " ++ String.fromInt toText.output.c ] ], div [ class "col-md-5" ] [ span [ class "h6 text-light" ] [ text <| "Caractéres sem espaço: " ++ String.fromInt toText.output.cse ] ], div [ class "col-md-3" ] [ span [ class "h6 text-light" ] [ text <| "Palavras: " ++ String.fromInt toText.output.p ] ] ]
        , div [ class "row mb-3", style "margin-left" "5px" ] [ div [ class "col-md-4 col-sm-12" ] [ span [ class "h6 text-light" ] [ text <| "Espaços: " ++ String.fromInt toText.output.e ] ], div [ class "col-md-5" ] [ span [ class "h6 text-light" ] [ text <| "Linhas: " ++ String.fromInt toText.output.l ] ], div [ class "col-md-3" ] [ span [ class "h6 text-light" ] [ text <| "Especial: " ++ String.fromInt toText.output.es ] ] ]
        , div [ class "row mb-3", style "margin-left" "5px" ] [ div [ class "col-md-4 col-sm-12" ] [ span [ class "h6 text-light" ] [ text <| "Vogais: " ++ String.fromInt toText.output.v ] ], div [ class "col-md-5" ] [ span [ class "h6 text-light" ] [ text <| "Consoantes: " ++ String.fromInt toText.output.con ] ], div [ class "col-md-3" ] [ span [ class "h6 text-light" ] [ text <| "Números: " ++ String.fromInt toText.output.n ] ] ]
        ]
