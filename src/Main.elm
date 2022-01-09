module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Json.Decode as Decode
import Maybe exposing (withDefault)
import Random
import Svg exposing (svg)
import Svg.Attributes exposing (..)
import Time


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Vec =
    { x : Int, y : Int }


vecsum : Vec -> Vec -> Vec
vecsum a b =
    Vec (a.x + b.x) (a.y + b.y)


type Msg
    = Nop
    | ToggleGamestate
    | Move Vec
    | UpdateFood Vec


type GameState
    = Pause
    | Play
    | Lose


type alias Model =
    { gamestate : GameState
    , snake : List Vec
    , direction : Vec
    , food : Vec
    }


field : Vec
field =
    Vec 30 20


init : () -> ( Model, Cmd Msg )
init () =
    ( { gamestate = Play
      , snake = List.map (\i -> Vec (field.x // 2 - i) (field.y // 2)) (List.range 0 4)
      , direction = Vec 1 0
      , food = Vec 6 6
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div [ style "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%);" ]
        [ p [] [ text (viewgamestate model.gamestate) ]
        , svg
            [ width <| String.fromInt <| 10 * field.x
            , height <| String.fromInt <| 10 * field.y
            , style "border: thick solid #000000"
            ]
            [ Svg.polyline
                [ points <| viewsnake model.snake
                , fill "none"
                , stroke "red"
                , strokeWidth "8"
                ]
                []
            , Svg.circle [ cx (10 * model.food.x |> String.fromInt), cy (10 * (field.y - model.food.y) |> String.fromInt), r "4" ] []
            ]
        ]


viewgamestate : GameState -> String
viewgamestate gs =
    case gs of
        Play ->
            "Arrows to Play, space to Pause."

        Lose ->
            "You lost! Press space to restart"

        Pause ->
            "Paused. Press space to continue"


viewsnake : List Vec -> String
viewsnake snake =
    List.foldr (\p s -> s ++ String.fromInt (10 * p.x) ++ "," ++ String.fromInt (10 * (field.y - p.y)) ++ " ") "" snake


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( model.gamestate, message ) of
        ( _, Nop ) ->
            ( model, Cmd.none )

        ( Pause, ToggleGamestate ) ->
            ( { model | gamestate = Play }, Cmd.none )

        ( Play, ToggleGamestate ) ->
            ( { model | gamestate = Pause }, Cmd.none )

        ( Pause, _ ) ->
            ( model, Cmd.none )

        ( Lose, ToggleGamestate ) ->
            init ()

        ( Lose, _ ) ->
            ( model, Cmd.none )

        ( Play, UpdateFood newpos ) ->
            ( { model | food = newpos }, Cmd.none )

        ( Play, Move dir ) ->
            let
                currhead =
                    List.head model.snake |> withDefault (Vec 0 0)

                nexthead =
                    vecsum currhead dir

                nextsnake =
                    if iseatingfood then
                        nexthead :: model.snake

                    else
                        nexthead :: List.take (List.length model.snake - 1) model.snake

                iseatingfood =
                    nexthead == model.food

                iseatingitself =
                    List.any ((==) nexthead) model.snake

                isoffbounds =
                    not (0 <= nexthead.x && nexthead.x <= field.x && 0 <= nexthead.y && nexthead.y <= field.y)
            in
            if isoffbounds || iseatingitself then
                ( { model | snake = nextsnake, direction = dir, gamestate = Lose }, Cmd.none )

            else if iseatingfood then
                ( { model | snake = nextsnake, direction = dir }, generatefood )

            else
                ( { model | snake = nextsnake, direction = dir }, Cmd.none )


generatefood : Cmd Msg
generatefood =
    Random.generate (\( x, y ) -> UpdateFood <| Vec x y) (Random.pair (Random.int 0 field.x) (Random.int 0 field.y))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.gamestate == Play then
            Time.every 1000
                (\_ ->
                    Move model.direction
                )

          else
            Sub.none
        , Browser.Events.onKeyDown (keydecoder model)
        ]


keydecoder : Model -> Decode.Decoder Msg
keydecoder model =
    Decode.map
        (\s ->
            let
                newmessage =
                    case s of
                        "ArrowRight" ->
                            Move (Vec 1 0)

                        "ArrowLeft" ->
                            Move (Vec -1 0)

                        "ArrowUp" ->
                            Move (Vec 0 1)

                        "ArrowDown" ->
                            Move (Vec 0 -1)

                        " " ->
                            ToggleGamestate

                        _ ->
                            Nop

                newd =
                    case newmessage of
                        Move dir ->
                            dir

                        _ ->
                            model.direction

                d =
                    model.direction
            in
            if d.x * newd.y - d.y * newd.x == 0 && d.x * newd.x + d.y * newd.y <= 0 then
                Nop

            else
                newmessage
        )
        (Decode.field "key" Decode.string)
