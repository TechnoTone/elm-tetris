module Main exposing (Model, Msg(..), initModel, main, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import GameGrid exposing (Msg)
import Html exposing (button, div, h1, h3, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Events.Extra.Touch as Touch exposing (Touch)
import Json.Decode as Decode
import PlayerAction
import Task
import Time



{-
   @@   @@  @@@@  @@@@@  @@@@@@ @@
   @@@ @@@ @@  @@ @@  @@ @@     @@
   @@ @ @@ @@  @@ @@  @@ @@@@   @@
   @@   @@ @@  @@ @@  @@ @@     @@
   @@   @@  @@@@  @@@@@  @@@@@@ @@@@@@
-}


type alias Model =
    { gameGrid : GameGrid.GameGridModel
    , gamePhase : Phase
    , gameData : GameData
    , touches : List Touch
    , viewportSize : ( Float, Float )
    }


type alias GameData =
    { speed : Int
    , score : Int
    , blockCount : Int
    , eliminationCount : Int
    }


type Phase
    = TitleScreen
    | Playing
    | GameOver Int


type Msg
    = GotViewport Dom.Viewport
    | WindowResize Int Int
    | Tick Time.Posix
    | StartGame
    | PlayerAction PlayerAction.Action
    | Touch TouchEventType Touch.Event
    | GameGridMsg GameGrid.Msg


type TouchEventType
    = TouchStart
    | TouchMove
    | TouchEnd


initModel : () -> ( Model, Cmd Msg )
initModel =
    always <|
        ( { gameGrid = GameGrid.uninitialised
          , gamePhase = TitleScreen
          , gameData = defaultGameData
          , touches = []
          , viewportSize = ( 0, 0 )
          }
        , Task.perform GotViewport Dom.getViewport
        )


defaultGameData : GameData
defaultGameData =
    { speed = startSpeed
    , score = 0
    , blockCount = 0
    , eliminationCount = 0
    }


startSpeed : Int
startSpeed =
    300



{-
   @@  @@ @@@@@@ @@@@@@ @@   @@
   @@  @@   @@   @@     @@   @@
   @@  @@   @@   @@@@   @@ @ @@
    @@@@    @@   @@     @@@@@@@
     @@   @@@@@@ @@@@@@  @@ @@
-}


view : Model -> Browser.Document Msg
view model =
    let
        heading =
            h1 [] [ text "TETRIS" ]

        viewGame =
            [ h3 [] [ text ("SCORE: " ++ String.fromInt model.gameData.score) ]
            , GameGrid.view model.gameGrid
                |> Html.map GameGridMsg
            ]

        ( classList, content ) =
            case model.gamePhase of
                TitleScreen ->
                    ( [ class "TitleScreen" ]
                    , [ button [ onClick StartGame ] [ text "START GAME" ] ]
                    )

                GameOver _ ->
                    ( [ class "GameOverScreen" ]
                    , viewGame
                        ++ [ div
                                [ class "GameOverPanel" ]
                                [ div [] [ text "GAME OVER" ] ]
                           ]
                    )

                Playing ->
                    ( [ Touch.onStart <| Touch TouchStart
                      , Touch.onMove <| Touch TouchMove
                      , Touch.onEnd <| Touch TouchEnd
                      ]
                    , viewGame
                    )
    in
    { title = "Tetris"
    , body =
        [ div
            (id "main" :: classList)
            (heading :: content)
        ]
    }



{-
   @@  @@ @@@@@  @@@@@   @@@@  @@@@@@ @@@@@@
   @@  @@ @@  @@ @@  @@ @@  @@   @@   @@
   @@  @@ @@@@@  @@  @@ @@@@@@   @@   @@@@
   @@  @@ @@     @@  @@ @@  @@   @@   @@
    @@@@  @@     @@@@@  @@  @@   @@   @@@@@@
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noUpdate : ( Model, Cmd msg )
        noUpdate =
            ( model, Cmd.none )

        updateModel : (Model -> Model) -> ( Model, Cmd msg )
        updateModel fn =
            ( fn model, Cmd.none )

        startGame =
            setGameGrid GameGrid.uninitialised >> setGameData defaultGameData >> setPhase Playing
    in
    case ( msg, model.gamePhase ) of
        ( GotViewport viewPort, _ ) ->
            updateModel (setViewport viewPort)

        ( WindowResize width height, _ ) ->
            updateModel (setWindowSize width height)

        ( StartGame, _ ) ->
            updateModel startGame

        ( PlayerAction PlayerAction.Drop, TitleScreen ) ->
            updateModel startGame

        ( PlayerAction action, Playing ) ->
            updateModel
                (setGameGrid
                    (GameGrid.handleAction action model.gameGrid)
                )

        ( PlayerAction PlayerAction.Drop, GameOver _ ) ->
            updateModel (setPhase TitleScreen)

        ( Touch eventType eventData, _ ) ->
            ( handleTouch eventType eventData model, Cmd.none )

        ( Tick posix, Playing ) ->
            let
                ms =
                    Time.posixToMillis posix
            in
            updateModel
                (setGameGrid (GameGrid.tick ms model.gameGrid))

        ( Tick posix, GameOver since ) ->
            if since + 5000 <= Time.posixToMillis posix then
                updateModel (setPhase TitleScreen)

            else
                noUpdate

        ( GameGridMsg ggMsg, _ ) ->
            updateModel
                (setGameGrid (GameGrid.update ggMsg model.gameGrid))

        _ ->
            noUpdate


handleTouch : TouchEventType -> Touch.Event -> Model -> Model
handleTouch eventType eventData model =
    case eventType of
        TouchStart ->
            model |> updateTouches (addTouches eventData.changedTouches)

        TouchMove ->
            model

        TouchEnd ->
            model
                |> convertTouches eventData.changedTouches
                |> List.foldr GameGrid.handleAction model.gameGrid
                |> setGameGridOn model
                |> updateTouches (removeTouches eventData.changedTouches)


updateTouches : (List Touch -> List Touch) -> Model -> Model
updateTouches fn model =
    { model | touches = fn model.touches }


addTouches : List Touch -> List Touch -> List Touch
addTouches newTouches existingTouches =
    List.append existingTouches newTouches


removeTouches : List Touch -> List Touch -> List Touch
removeTouches toRemove existingTouches =
    let
        ids =
            toRemove |> List.map .identifier
    in
    existingTouches |> List.filter (\t -> not <| List.member t.identifier ids)


convertTouches : List Touch -> Model -> List PlayerAction.Action
convertTouches endedTouches { touches, viewportSize } =
    let
        matchingTouch : Touch -> Touch
        matchingTouch t =
            touches |> List.filter (.identifier >> (==) t.identifier) |> List.head |> Maybe.withDefault t

        convertTouch : Touch -> PlayerAction.Action
        convertTouch touch =
            let
                touchX =
                    Tuple.first touch.clientPos

                clientWidth =
                    Tuple.first viewportSize

                match : Touch
                match =
                    matchingTouch touch

                distance : ( Float, Float ) -> ( Float, Float ) -> Float
                distance ( x1, y1 ) ( x2, y2 ) =
                    sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

                convertSwipe : ( Float, Float ) -> ( Float, Float ) -> PlayerAction.Action
                convertSwipe ( x1, y1 ) ( x2, y2 ) =
                    let
                        dx =
                            x1 - x2

                        dy =
                            y1 - y2
                    in
                    if abs dx > abs dy then
                        if dx > 0 then
                            PlayerAction.Right

                        else
                            PlayerAction.Left

                    else if dy > 0 then
                        PlayerAction.RotateRight

                    else
                        PlayerAction.RotateLeft
            in
            if distance touch.clientPos match.clientPos < 8 then
                if touchX < clientWidth / 3 then
                    PlayerAction.Left

                else if touchX < clientWidth / 3 * 2 then
                    PlayerAction.Down

                else
                    PlayerAction.Right

            else
                convertSwipe touch.clientPos match.clientPos
    in
    List.map convertTouch endedTouches


setViewport : Dom.Viewport -> Model -> Model
setViewport viewport model =
    { model | viewportSize = ( viewport.scene.width, viewport.scene.height ) }


setWindowSize : Int -> Int -> Model -> Model
setWindowSize width height model =
    { model | viewportSize = ( toFloat width, toFloat height ) }


setPhase : Phase -> Model -> Model
setPhase phase model =
    { model | gamePhase = phase }


setGameGrid : GameGrid.GameGridModel -> Model -> Model
setGameGrid gameGrid model =
    { model | gameGrid = gameGrid }


setGameGridOn : Model -> GameGrid.GameGridModel -> Model
setGameGridOn model gameGrid =
    setGameGrid gameGrid model


setGameData : GameData -> Model -> Model
setGameData gameData model =
    { model | gameData = gameData }



{-
   @@   @@   @@@@   @@@@@@  @@  @@
   @@@ @@@  @@  @@    @@    @@@ @@
   @@ @ @@  @@@@@@    @@    @@ @@@
   @@   @@  @@  @@    @@    @@  @@
   @@   @@  @@  @@  @@@@@@  @@  @@
-}


main : Program () Model Msg
main =
    Browser.document
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gamePhase of
        Playing ->
            Sub.batch
                [ Browser.onAnimationFrame Tick
                , Browser.onKeyDown (Decode.map PlayerAction keyDecoder)
                , Browser.onResize WindowResize
                ]

        _ ->
            Sub.batch
                [ Browser.onKeyDown (Decode.map PlayerAction keyDecoder)
                , Browser.onResize WindowResize
                ]


keyDecoder : Decode.Decoder PlayerAction.Action
keyDecoder =
    let
        toAction string =
            case String.toUpper string of
                "ARROWLEFT" ->
                    PlayerAction.Left

                "A" ->
                    PlayerAction.Left

                "ARROWRIGHT" ->
                    PlayerAction.Right

                "D" ->
                    PlayerAction.Right

                "Q" ->
                    PlayerAction.RotateLeft

                "E" ->
                    PlayerAction.RotateRight

                "ARROWUP" ->
                    PlayerAction.RotateRight

                "ARROWDOWN" ->
                    PlayerAction.Down

                "S" ->
                    PlayerAction.Down

                " " ->
                    PlayerAction.Drop

                _ ->
                    PlayerAction.None
    in
    Decode.map toAction (Decode.field "key" Decode.string)
