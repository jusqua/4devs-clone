module GeradorNumero exposing (main)
import Browser exposing (element)
import Html exposing (Html, div, text, h5, span, button, input)
import Html.Attributes exposing (class, style, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Random

-- Types
type alias Model = {o : String, i1 : String, i2 : String}
type Msg = Output Int | Input1 String | Input2 String | Button

-- Variables
toText = 
  {
    title = "Gerador de Números",
    box = "Valor",
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

init : () -> (Model, Cmd Msg)
init _ = (Model "" "" "", Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Input1 u ->
      ({model | i1 = u}, Cmd.none)
    Input2 u ->
      ({model | i2 = u}, Cmd.none)
    Button ->
      case (String.toInt model.i1, String.toInt model.i2) of
        (Just x, Just y) ->
          (model, Random.generate Output <| Random.int x y)
        _ -> (model, Cmd.none)
    Output u ->
      ({model | o = String.fromInt u }, Cmd.none)

view : Model -> Html Msg
view model =
  div [ class "card col-md-4 col-sm-12 bg-dark" ]
    [ div [ class "card-body bg-dark text-light" ]
      [ h5 [ class "card-title" ] [ text toText.title ]
      , div [ class "row"] [ 
         div [ class "col-md-6" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box, type_ "number", onInput Input1 ] [] ]
        , div [ class "col-md-6" ] [ input [ class "form-control bg-dark text-light", placeholder toText.box, type_ "number", onInput Input2 ] [] ]]
        ,div [class "row"] [div [class "cow-md-12"] [button [ class "btn btn-outline-light mt-2", style "width" "100%", onClick Button] [ text toText.btn ]]]
        ,   span [class "h6"] [text "Resultado: "] , text <| model.o
        ] ] 
