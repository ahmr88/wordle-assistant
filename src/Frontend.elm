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
    ( { guesses = Arr.fromList [], clientId = "" }, Cmd.none )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        EnterKeyPressed ->
            case Arr.get (Arr.length model.guesses - 1) model.guesses of
                Nothing ->
                    ( model, Cmd.none )

                Just gs ->
                    if length gs == 5 then
                        ( { model
                            | guesses =
                                Arr.push "" model.guesses
                          }
                        , Debug.log "sendToBack" Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        DelKeyPressed ->
            case Arr.get (Arr.length model.guesses - 1) model.guesses of
                Nothing ->
                    ( model, Cmd.none )

                Just gs ->
                    if length gs == 1 then
                        ( { model
                            | guesses = Arr.slice 0 -1 model.guesses
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | guesses = Arr.set 
                                          (Arr.length model.guesses - 1) 
                                          (fromList 
                                            <| List.reverse 
                                            <| List.drop 1 
                                            <| List.reverse 
                                            <| toList gs
                                          ) model.guesses
                          }
                        , Cmd.none
                        )

        CharKeyPressed c ->
            case Arr.get (Arr.length model.guesses - 1) model.guesses of
                Nothing ->
                    ( { model | guesses = Arr.fromList [fromList [ c ]] }, Cmd.none )

                Just gs ->
                    if length gs < 5 then
                        ( { model | guesses = Arr.set (Arr.length model.guesses - 1) (gs ++ fromList [c]) model.guesses
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        --     ( { model
        --         | guesses =
        --             model.guesses
        --                 ++ List.singleton
        --                     (fromList [ c ])
        --       }
        --     , Cmd.none
        --     )
        FNoop ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    ( model, Cmd.none )


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
                        [ inputArea <| Arr.toList model.guesses
                        , infoArea
                        ]
                , keyboardArea
                ]
        ]


inputArea : List String -> Element msg
inputArea gss =
    row [] [ column [ spacing 10 ] <| List.map singleInputRow <| fillList 6 "" gss ]


fillList : Int -> a -> List a -> List a
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
    aux big xs


singleInputRow : String -> Element msg
singleInputRow =
    row [ spacing 10 ]
        << List.map singleInput
        << fillList 5 ""
        << List.map
            fromChar
        << toList


singleInput : String -> Element msg
singleInput s =
    el
        [ Font.size 30
        , Font.light
        , Bg.color (rgb255 59 59 60)

        -- , Bg.color (rgb255 82 141 71)
        -- , Bg.color (rgb255 181 159 59)
        , Border.rounded 5
        , width (px 40)
        , height (px 50)
        , htmlAttribute <| HA.style "text-transform" "uppercase"
        ]
    <|
        el [ centerX, centerY ] <|
            text s


infoArea =
    row [ height fill, centerX ] [ text "3000 words remaining" ]


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
