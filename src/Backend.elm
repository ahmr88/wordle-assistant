module Backend exposing (app, init)

import Array as Arr
import Dict
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Set exposing (Set)
import String exposing (..)
import Task
import Types exposing (..)
import Wordle exposing (wordleWords)


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
    ( { clientGuesses = Dict.empty }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            ( { model
                | clientGuesses = Dict.insert clientId [] model.clientGuesses
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        SubmitGuesses gss ->
            let
                newGuesses =
                    Dict.update clientId (\mbv -> Just gss) model.clientGuesses

                possibles =
                    possibleGuesses wordleWords gss
            in
            ( { model | clientGuesses = newGuesses }
            , sendToFrontend clientId <|
                if List.length possibles > 50 then
                    FilteredWordsCount
                        (List.length
                            possibles
                        )

                else
                    FilteredWords possibles
            )


subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        ]


getElim g =
    case g of
        Elim c ->
            Just c

        _ ->
            Nothing


getCont g =
    case g of
        Contains c ->
            Just c

        _ ->
            Nothing


getAt g =
    case g of
        At c i ->
            Just ( i, c )

        _ ->
            Nothing


unwrapMaybes : List (Maybe a) -> List a
unwrapMaybes mbs =
    case mbs of
        [] ->
            []

        (Just x) :: bs ->
            x :: unwrapMaybes bs

        Nothing :: bs ->
            unwrapMaybes bs


possibleGuesses : List String -> List (Guess Char) -> List String
possibleGuesses ws gss =
    let
        elims =
            unwrapMaybes <| List.map getElim gss

        conts =
            unwrapMaybes <| List.map getCont gss

        ats =
            unwrapMaybes <| List.map getAt gss
    in
    List.filter (\cand -> List.all (\elim -> not <| List.member elim <| toList cand) elims) <|
        List.filter (\cand -> List.all (\con -> List.member con <| toList cand) conts) <|
            List.filter (\cand -> List.all (\( i, c ) -> List.member ( i, c ) <| Arr.toIndexedList <| Arr.fromList <| toList cand) ats) <|
                ws


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity
