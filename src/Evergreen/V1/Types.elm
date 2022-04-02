module Evergreen.V1.Types exposing (..)

import Array
import Dict
import Lamdera


type Guess a
    = Elim a
    | NotAt a Int
    | At a Int


type alias FrontendModel =
    { guesses : Array.Array (Array.Array (Guess Char))
    , clientId : String
    , possibleGuesses : Maybe (List String)
    , possibleGuessesCount : Maybe Int
    }


type alias BackendModel =
    { clientGuesses : Dict.Dict String (List (Guess Char))
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
    = ClientConnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = FilteredWords (List String)
    | FilteredWordsCount Int
