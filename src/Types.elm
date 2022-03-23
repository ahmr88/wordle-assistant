
  
module Types exposing (..)

import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)


type alias BackendModel =
    { questions : List String
    }


type alias FrontendModel =
    { questions : List String
    , qInput : String
    , clientId : String
    }


type FrontendMsg
      = AddQuestion
      | InputChange String
      | FNoop


type ToBackend
      = QuestionAdded (Maybe String)


type BackendMsg
    = ClientConnected SessionId ClientId
    | Noop

type ToFrontend
    = QuestionsChanged (List String) String
