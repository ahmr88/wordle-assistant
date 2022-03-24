module Frontend exposing (Model, app)

import Array as Arr
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA
import Lamdera exposing (sendToBackend)
import Maybe exposing (..)
import String exposing (..)
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
    ( { guesses = Arr.fromList []
      , possibleGuesses = Nothing
      , clientId = ""
      , possibleGuessesCount = Nothing
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        EnterKeyPressed ->
            case Arr.get (Arr.length model.guesses - 1) model.guesses of
                Nothing ->
                    ( model, Cmd.none )

                Just gs ->
                    if Arr.length gs == 5 then
                        ( { model
                            | guesses =
                                Arr.push Arr.empty model.guesses
                          }
                        , sendToBackend <|
                            SubmitGuesses
                                (List.concat <|
                                    Arr.toList <|
                                        Arr.map Arr.toList model.guesses
                                )
                        )

                    else
                        ( model, Cmd.none )

        DelKeyPressed ->
            case Arr.get (Arr.length model.guesses - 1) model.guesses of
                Nothing ->
                    ( model, Cmd.none )

                Just gs ->
                    if Arr.length gs <= 1 then
                        ( { model
                            | guesses = Arr.slice 0 -1 model.guesses
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | guesses =
                                Arr.set
                                    (Arr.length model.guesses - 1)
                                    (Arr.slice 0 -1 gs)
                                    model.guesses
                          }
                        , Cmd.none
                        )

        CharKeyPressed c ->
            case Arr.get (Arr.length model.guesses - 1) model.guesses of
                Nothing ->
                    ( { model | guesses = Arr.fromList [ Arr.fromList [ Elim c ] ] }, Cmd.none )

                Just gs ->
                    if Arr.length gs < 5 then
                        ( { model
                            | guesses =
                                Arr.set (Arr.length model.guesses - 1)
                                    (Arr.push (Elim c) gs)
                                    model.guesses
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        GuessStateChange row col ->
            if row /= Arr.length model.guesses - 1 then
                ( model, Cmd.none )

            else
                case
                    Arr.get row model.guesses
                        |> andThen
                            (\r -> Arr.get col r |> andThen (\g -> Just ( r, g )))
                of
                    Nothing ->
                        ( model, Cmd.none )

                    Just ( r, Elim c ) ->
                        ( { model | guesses = Arr.set row (Arr.set col (Contains c) r) model.guesses }, Cmd.none )

                    Just ( r, Contains c ) ->
                        ( { model | guesses = Arr.set row (Arr.set col (At c col) r) model.guesses }, Cmd.none )

                    Just ( r, At c i ) ->
                        ( { model | guesses = Arr.set row (Arr.set col (Elim c) r) model.guesses }, Cmd.none )

        FNoop ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        FilteredWords ws ->
            ( { model | possibleGuesses = Just ws }, Cmd.none )

        FilteredWordsCount i ->
            ( { model | possibleGuessesCount = Just i }, Cmd.none )


view : Model -> Html FrontendMsg
view model =
    layout
        [ Font.family
            [ Font.monospace
            ]
        ]
    <|
        column
            [ width fill
            , height fill
            , padding 20
            , Bg.color <|
                rgb255 18 18 19
            , Font.color <| rgb 242 239 223
            ]
            [ titleBar, mainContent model ]


titleBar =
    row
        [ width fill
        , padding 20
        , Font.size 26
        , Font.heavy
        , Border.width 2
        ]
        [ el [ centerX ] <| text "Wordle Assistant" ]


mainContent model =
    row
        [ width fill
        , height fill
        ]
        [ el [ centerX, height fill, padding 10 ] <|
            column
                [ height fill
                ]
                [ el
                    [ height fill
                    , centerX
                    , paddingXY 0 20
                    ]
                  <|
                    column [ spacing 30 ]
                        [ inputArea model.guesses
                        , infoArea model
                        ]
                , keyboardArea
                ]
        ]


inputArea gss =
    row []
        [ column [ spacing 10 ] <|
            List.map
                (\( r, gs ) ->
                    singleInputRow r gs
                )
            <|
                Arr.toIndexedList <|
                    fillList 6 Arr.empty gss
        ]


fillList : Int -> a -> Arr.Array a -> Arr.Array a
fillList len filler xs =
    let
        big =
            List.repeat len filler

        aux bg sm =
            case ( bg, sm ) of
                ( [], ss ) ->
                    ss

                ( bs, [] ) ->
                    List.repeat (List.length bs) filler

                ( b :: bs, s :: ss ) ->
                    s :: aux bs ss
    in
    Arr.fromList <| aux big (Arr.toList xs)


singleInputRow r =
    row [ spacing 10 ]
        << List.map (\( c, g ) -> singleInput r c g)
        << Arr.toIndexedList
        << fillList 5 Nothing
        << Arr.map Just


singleInput row col s =
    Input.button
        [ Font.size 30
        , Font.light
        , case s of
            Nothing ->
                Bg.color (rgb255 59 59 60)

            Just (Elim _) ->
                Bg.color (rgb255 59 59 60)

            Just (Contains _) ->
                Bg.color (rgb255 181 159 59)

            Just (At _ _) ->
                Bg.color (rgb255 82 141 71)
        , Border.rounded 5
        , width (px 40)
        , height (px 50)
        , htmlAttribute <| HA.style "text-transform" "uppercase"
        ]
        { label =
            el [ centerX, centerY ] <|
                text
                    (case s of
                        Nothing ->
                            ""

                        Just (Contains c) ->
                            fromChar c

                        Just (Elim c) ->
                            fromChar c

                        Just (At c _) ->
                            fromChar c
                    )
        , onPress = Just (GuessStateChange row col)
        }


infoArea model =
    let
        content =
            case model.possibleGuesses of
                Just gss -> List.map (\x -> row [] [text x]) gss
                    -- (fromInt <| List.length x) ++ " words remain"

                Nothing ->
                    case model.possibleGuessesCount of
                        Just i ->
                            [row [] [ text <| fromInt i ++ " words remain"]]
                        Nothing -> []
    in
    column [ height fill, centerX, Font.size 16 ] 
      content


keyboardArea =
    row [ width fill ]
        [ column [ spacing 10, centerX ] <|
            List.map
                (keyRow
                    << keySetGenerator
                )
                [ "qwertyuiop", "asdfghjkl" ]
                ++ [ lastRow ]
        ]


keyRow keyEls =
    row [ centerX, spacing 5 ] keyEls


lastRow =
    keyRow <|
        [ singleKey "Enter" EnterKeyPressed ]
            ++ keySetGenerator "zxcvbnm"
            ++ [ singleKey "Del" DelKeyPressed ]


keySetGenerator =
    List.map (\c -> singleKey (fromChar c) (CharKeyPressed c)) << toList


singleKey st msg =
    Input.button
        [ Bg.color (rgb255 59 59 60)
        , Border.rounded 10
        ]
        { label = el [ paddingXY 5 10 ] <| text st
        , onPress = Just msg
        }



-- el [ explain Debug.todo, width fill, height fill ] <|
--     column [ explain Debug.todo, Bg.color (rgb255 0 148 240) ] <|
--         List.map (el [ Font.color (rgb 1 1 1) ]) [ text "hello", text "world" ]
-- Html.div [ style "padding" "30px" ]
--     [ Html.form [onSubmit AddQuestion] [
--         Html.input [placeholder "Enter your question", value model.qInput, onInput InputChange] []
--       , Html.button [ onClick AddQuestion ] [ text "Submit" ]
--     ]
--     , Html.text (String.fromInt <| List.length model.questions)
--     , Html.ul [] <| List.map (\q -> Html.div [] [Html.text q]) model.questions
--     ]
