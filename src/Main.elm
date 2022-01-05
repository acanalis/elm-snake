module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (..)
import Json.Decode as Decode
import Maybe exposing (withDefault)
import Random
import Svg exposing (svg)
import Svg.Attributes exposing (..)
import Time


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Vec =
    { x : Int, y : Int }


zerovec : Vec
zerovec =
    Vec 0 0


vecsum : Vec -> Vec -> Vec
vecsum a b =
    Vec (a.x + b.x) (a.y + b.y)


vecdiff : Vec -> Vec -> Vec
vecdiff a b =
    Vec (a.x - b.x) (a.y - b.y)


vecmul : Int -> Vec -> Vec
vecmul p a =
    Vec (a.x * p) (a.y * p)


norm : Vec -> Vec
norm a =
    if a.x == 0 then
        Vec 0 (a.y // abs a.y)

    else
        Vec (a.x // abs a.x) 0


type Msg
    = Turn (Maybe Vec)
    | Step
    | UpdateFood Vec


type GameState
    = Pause
    | Play
    | Lose


type alias Model =
    { gamestate : GameState
    , snake : List Vec
    , food : Vec
    }


field =
    Vec 40 20


init : () -> ( Model, Cmd Msg )
init () =
    ( { gamestate = Play
      , snake = [ Vec (field.x // 2) (field.y // 2), Vec (field.x // 2 - 4) (field.y // 2) ]
      , food = Vec 6 6
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div [ style "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%);"]
        [ p [] [text (viewgamestate model.gamestate)],
          svg
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
            "Play with the Arrow keys"

        Lose ->
            "You lost! Press any key to start over"

        Pause ->
            "Pause"


viewsnake : List Vec -> String
viewsnake snake =
    List.foldr (\p s -> s ++ String.fromInt (10 * p.x) ++ "," ++ String.fromInt (10 * (field.y - p.y)) ++ " ") "" snake


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( model.gamestate, message ) of
        ( Pause, _ ) ->
            ( model, Cmd.none )

        ( Lose, Turn _ ) ->
            init ()

        ( Lose, _ ) ->
            ( model, Cmd.none )

        ( Play, msg ) ->
            let
                nextmodel =
                    advanceplayfun msg model

                nextmodelhead =
                    List.head nextmodel.snake |> withDefault zerovec

                isoffbounds =
                    not (0 <= nextmodelhead.x && nextmodelhead.x <= field.x && 0 <= nextmodelhead.y && nextmodelhead.y <= field.y)

                iseatingitself =
                    isonsnake (List.tail nextmodel.snake |> withDefault []) nextmodelhead
            in
            if isoffbounds || iseatingitself then
                ( { nextmodel | gamestate = Lose }, Cmd.none )

            else if isonsnake nextmodel.snake model.food then
                ( nextmodel, generatefood )

            else
                ( nextmodel, Cmd.none )


generatefood : Cmd Msg
generatefood =
    Random.generate (\( x, y ) -> UpdateFood <| Vec x y) (Random.pair (Random.int 0 field.x) (Random.int 0 field.y))

isonsnake : List Vec -> Vec -> Bool
isonsnake snake point =
    case snake of
        a :: b :: c ->
            let
                d =
                    vecdiff a point

                e =
                    vecdiff b point
            in
            if d.x * e.y - d.y * e.x == 0 && d.x * e.x + d.y * e.y <= 0 then
                --If the points are colineal, and point is between/equal to them
                True

            else
                isonsnake (b :: c) point

        _ ->
            False


advanceplayfun : Msg -> Model -> Model
advanceplayfun msg model =
    let
        head = List.head model.snake |> withDefault zerovec

        headdir =
            vecdiff head (List.tail model.snake |> Maybe.andThen List.head |> withDefault zerovec) |> norm

        nexthead =
            vecsum head headdir

        eating =
            nexthead == model.food
    in
    case msg of
        Turn direction ->
            case direction of
                Nothing ->
                    model

                Just dir ->
                    if dir.x * headdir.x + dir.y * headdir.y >= 0 then
                        { model | snake = (vecsum head dir)::model.snake |> steptail eating }

                    else
                        model

        Step ->
            { model | snake = nexthead::(List.tail model.snake |> withDefault []) |> steptail eating }

        UpdateFood newpos ->
            { model | food = newpos }


steptail : Bool -> List Vec -> List Vec
steptail eating snake =
    let
        revsnake = List.reverse snake
        tail =
            List.head revsnake |> withDefault zerovec

        pretail =
            List.tail revsnake |> Maybe.andThen List.head |> withDefault zerovec

        taildir =
            vecdiff pretail tail |> norm

        nexttail =
            vecsum tail taildir
    in
    if eating then
        snake

    else if pretail == nexttail && List.length revsnake /= 2 then
        List.reverse (List.tail revsnake|> withDefault [])

    else
        List.reverse (nexttail::(List.tail revsnake |> withDefault []))

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 (\_ -> Step)
        , Browser.Events.onKeyDown keydecoder
        ]


keydecoder : Decode.Decoder Msg
keydecoder =
    Decode.map
        (\s ->
            case s of
                "ArrowRight" ->
                    Turn (Just (Vec 1 0))

                "ArrowLeft" ->
                    Turn (Just (Vec -1 0))

                "ArrowUp" ->
                    Turn (Just (Vec 0 1))

                "ArrowDown" ->
                    Turn (Just (Vec 0 -1))

                _ ->
                    Turn Nothing
        )
        (Decode.field "key" Decode.string)
