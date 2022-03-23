module Evergreen.V1.Types exposing (..)

import Lamdera


type alias FrontendModel =
    { questions : (List String)
    , qInput : String
    , clientId : String
    }


type alias BackendModel =
    { questions : (List String)
    }


type FrontendMsg
    = AddQuestion
    | InputChange String
    | FNoop


type ToBackend
    = QuestionAdded (Maybe String)


type BackendMsg
    = ClientConnected Lamdera.SessionId Lamdera.ClientId
    | Noop


type ToFrontend
    = QuestionsChanged (List String) String