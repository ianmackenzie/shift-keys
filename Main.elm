module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)


type alias Model =
    { rightShiftCount : Int
    , leftShiftCount : Int
    , unknownShiftCount : Int
    }


model : Model
model =
    { rightShiftCount = 0
    , leftShiftCount = 0
    , unknownShiftCount = 0
    }


type Msg
    = UnkownShiftUp
    | LeftShiftUp
    | RightShiftUp


keyEventDecoder : Decoder Msg
keyEventDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Shift" then
                    Decode.field "location" Decode.int
                        |> Decode.andThen
                            (\location ->
                                case location of
                                    1 ->
                                        Decode.succeed LeftShiftUp

                                    2 ->
                                        Decode.succeed RightShiftUp

                                    _ ->
                                        Decode.succeed UnkownShiftUp
                            )
                else
                    Decode.fail "Ignore non-shift keys"
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text "Type in here to see how often you use different Shift keys!" ]
        , Html.div [] [ Html.textarea [ Events.on "keyup" keyEventDecoder ] [] ]
        , Html.div [] [ Html.text (toString model.rightShiftCount ++ " right shift presses") ]
        , Html.div [] [ Html.text (toString model.leftShiftCount ++ " left shift presses") ]
        , Html.div [] [ Html.text (toString model.unknownShiftCount ++ " unknown shift presses") ]
        ]


update : Msg -> Model -> Model
update message model =
    case message of
        LeftShiftUp ->
            { model | leftShiftCount = model.leftShiftCount + 1 }

        RightShiftUp ->
            { model | rightShiftCount = model.rightShiftCount + 1 }

        UnkownShiftUp ->
            { model | unknownShiftCount = model.unknownShiftCount + 1 }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }
