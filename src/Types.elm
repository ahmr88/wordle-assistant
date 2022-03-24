module Types exposing (..)

import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Array exposing (Array)


type alias BackendModel =
    { guesses : List String
    , clients : List String
    }


type alias FrontendModel =
    { guesses : Array (Array (Guess Char))
    , clientId : String
    }


type FrontendMsg
    = EnterKeyPressed
    | DelKeyPressed
    | CharKeyPressed Char
    | GuessStateChange Int Int
    | FNoop


type ToBackend
    = SubmitGuesses (List (Guess Char))


type BackendMsg
    = ClientConnected SessionId ClientId


type ToFrontend
    = FilteredWords ClientId (List String)

type Guess a  = Elim a | Contains a | At a Int
