module SubstituirLinha exposing (main)
import Browser exposing (sandbox)
import Html exposing (Html, div, text, h5, input, textarea, span)
import Html.Attributes exposing (class, placeholder, style)
import Html.Events exposing (onInput)

-- Types
type alias Model = {i1 : String, i2 : String}
type Msg = Input1 String | Input2 String

-- Function
substituirLinha : String -> String -> String
substituirLinha text ntext =
  let
    aux : List Char -> List Char -> List Char
    aux ls ts =
      case ls of
        [] -> []
        x::xs -> 
          if x == '\n' then List.append ts (aux xs ts)
          else x::(aux xs ts)
  in
    String.fromList <| aux (String.toList text) (String.toList ntext)

-- Variables
toText =
  {
    title = "Substituir Quebra de Linha",
    box1 = "Texto Original",
    box2 = "Texto pelo qual será substituido"
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
    Input1 n ->
      {model | i1 = n}
    Input2 n ->
      {model | i2 = n}
  
view : Model -> Html Msg
view model =
  div [ class "card col-md-4 col-sm-12 bg-dark" ]
    [ div [ class "card-body bg-dark text-light" ]
      [ h5 [ class "card-title" ] [ text toText.title ],
        div [] [ div [ class "row" ]
          [ div [ class "col-md-12"] [ textarea [class "form-control bg-dark text-light", placeholder toText.box1, onInput Input1, style "resize" "none" ] [] ] ],
            div [ class "col-md-12 mt-2" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box2, onInput Input2 ] [] ] ],
            div [ class "row" ] [ div [ class "col-md-12" ] [ span [ class "h6" ] [ text "Resultado: \n" ], text <| substituirLinha model.i1 model.i2 ] ] ] ]
