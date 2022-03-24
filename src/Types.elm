module Types exposing (..)

import Array exposing (Array)
import Lamdera exposing (ClientId, SessionId)
import Dict exposing (Dict)


type alias BackendModel =
    { clientGuesses : Dict String (List (Guess Char))
    }


type alias FrontendModel =
    { guesses : Array (Array (Guess Char))
    , clientId : String
    , possibleGuesses: Maybe (List String)
    , possibleGuessesCount: Maybe Int
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
    = FilteredWords (List String)
    | FilteredWordsCount Int


type Guess a
    = Elim a
    | Contains a
    | At a Int
