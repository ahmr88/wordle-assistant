module Frontend exposing (Model, app)

import Html exposing (Html, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Lamdera exposing (sendToBackend)
import Types exposing (..)


type alias Model =
    FrontendModel


{-| Lamdera applications define 'app' instead of 'main'.
Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.
-}
app =
    Lamdera.frontend
        { init = \_ _ -> init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "v1"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        , onUrlChange = \_ -> FNoop
        , onUrlRequest = \_ -> FNoop
        }


init : ( Model, Cmd FrontendMsg )
init =
    ( { questions = [], clientId = "", qInput = "" }, Cmd.none )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        AddQuestion ->
          if model.qInput == "" 
            then
              (model , Cmd.none)
            else
              ( { model | questions = model.qInput :: model.questions, qInput = ""}
              , sendToBackend <| QuestionAdded (Just model.qInput)
              )
        InputChange s -> ({ model | qInput = s }, Cmd.none)
        FNoop ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        QuestionsChanged qs clientId -> ( { model | questions = qs, clientId = clientId }, Cmd.none )


view : Model -> Html FrontendMsg
view model =
    Html.div [ style "padding" "30px" ]
        [ Html.form [onSubmit AddQuestion] [
            Html.input [placeholder "Enter your question", value model.qInput, onInput InputChange] []
          , Html.button [ onClick AddQuestion ] [ text "Submit" ]
        ]
        , Html.text (String.fromInt <| List.length model.questions)
        , Html.div [] [ Html.text "Click me then refresh me!" ]
        , Html.ul [] <| List.map (\q -> Html.div [] [Html.text q]) model.questions
        ]
