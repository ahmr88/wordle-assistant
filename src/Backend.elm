module Backend exposing (app, init)

import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Set exposing (Set)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { questions = [] }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            ( model
            , sendToFrontend clientId <| QuestionsChanged model.questions clientId 
            )
        Noop ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        QuestionAdded Nothing -> (model, Cmd.none)
        QuestionAdded (Just "remove all") -> ({model | questions = []}, broadcast (QuestionsChanged [] clientId))
        QuestionAdded (Just s) -> 
          let newQs = s :: model.questions
          in
            ( { model | questions = newQs }
            , broadcast (QuestionsChanged  newQs clientId) 
            )

subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        ]
