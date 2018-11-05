port module Main exposing (Model, Msg(..), checkArcMatch, getMatchingArcs, handleTrigger, init, initialModel, main, n1, n2, n3, n4, n5, subscriptions, update, view)

import Browser
import Debug exposing (log, toString)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as E
import Time
import Update.Extra exposing (sequence)



--port play : E.Value -> Cmd msg


port sounds : E.Value -> Cmd msg



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Msg
    = Tick Time.Posix
    | OximetryBeep Time.Posix
    | UpdateStringValue Key String
    | UpdateNumValue Key Float
    | DeltaNumValueByAmount Key Float
    | DeltaNumValueByPercent Key Float
    | HistoryRequest
    | ExaminationRequest
    | IVFluids FluidRate FluidType
    | O2Therapy FiO2
    | Trigger TriggerMsg
    | ProcessMessages ArcMessages
    | Pause
    | Run


type TriggerMsg
    = History
    | Examination


type DbValue
    = F Float
    | S String


type alias FluidType =
    String


type alias FluidRate =
    Int


type alias FiO2 =
    Float


type alias Key =
    String


type alias ArcName =
    String


type alias ArcMessages =
    List Msg


type alias NodeName =
    String


type Arc
    = Arc String (Maybe Msg) (List Msg) (() -> Node)


type alias Node =
    { name : NodeName
    , timeoutArc : Maybe Arc
    , arcs : List Arc
    , timeout : Maybe Int
    }


type RunningState
    = NotStarted
    | Running
    | Paused


initialData : Dict.Dict String DbValue
initialData =
    Dict.fromList
        [ ( "bp", F 85.0 )
        , ( "temp", F 37.0 )
        , ( "hr", F 100.0 )
        , ( "saO2", F 85 )
        , ( "ecg", S "SR 100" )
        ]


n0 =
    Node "START" (Just (Arc "n0_timeout" Nothing [] (\() -> n1))) [] (Just 0)


n1 =
    Node "N1"
        (Just (Arc "n1_timeout" Nothing [ UpdateNumValue "saO2" 75, UpdateStringValue "ecg" "AF at 150", UpdateNumValue "hr" 150 ] (\() -> n2)))
        [ Arc "n1_hist" (Just HistoryRequest) [ UpdateNumValue "saO2" 80, UpdateStringValue "ecg" "AF at 150", UpdateNumValue "hr" 150 ] (\() -> n2)
        , Arc "n1_exam" (Just ExaminationRequest) [ UpdateNumValue "saO2" 80, UpdateStringValue "ecg" "AF at 150", UpdateNumValue "hr" 150 ] (\() -> n2)
        , Arc "n1_o2" (Just (O2Therapy 0.3)) [ UpdateNumValue "saO2" 89, UpdateStringValue "ecg" "SR 120" ] (\() -> n3)
        ]
        (Just 60)


n2 =
    Node "N2"
        (Just (Arc "n2_timeout" Nothing [ UpdateNumValue "saO2" 75, UpdateNumValue "bp" 70, UpdateStringValue "ecg" "slowing AF @ 80", UpdateNumValue "hr" 80 ] (\() -> n4)))
        [ Arc "n1_o2" (Just (O2Therapy 0.3)) [ UpdateNumValue "saO2" 89, UpdateStringValue "ecg" "SR 120", UpdateNumValue "hr" 120 ] (\() -> n3) ]
        (Just 55)


n3 =
    Node
        "N3"
        (Just (Arc "n3_timeout" Nothing [ UpdateNumValue "saO2" 85, UpdateStringValue "ecg" "AF at 120", UpdateNumValue "hr" 120 ] (\() -> n2)))
        [ Arc "n3_fluids" (Just (IVFluids 100 "saline")) [ UpdateNumValue "bp" 95, UpdateStringValue "ecg" "SR at 90", UpdateNumValue "hr" 90 ] (\() -> n5) ]
        (Just 90)


n4 =
    Node "N4"
        Nothing
        []
        Nothing


n5 =
    Node "N5"
        Nothing
        []
        Nothing


type alias Model =
    { runningState : RunningState
    , elapsedSimTime : Int
    , timeInCurrentNode : Int
    , speedUp : Float
    , currentNode : Node
    , nodes : List Node
    , data : Dict String DbValue
    }


initialModel : Model
initialModel =
    { runningState = NotStarted
    , elapsedSimTime = 0
    , timeInCurrentNode = 0
    , speedUp = 1
    , currentNode = n1
    , nodes = [ n1, n2 ]
    , data = initialData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


updateData model key value =
    Dict.update key (\_ -> Just value) model.data


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OximetryBeep _ ->
            if model.runningState == Running then
                ( model, sounds (E.string "beep") )

            else
                ( model, Cmd.none )

        Tick _ ->
            if model.runningState == Running then
                model
                    |> updateSimTime
                    |> updateNodeTime
                    |> maybeTimeoutNode
                    |> (\newModel -> ( newModel, Cmd.none ))

            else
                ( model, Cmd.none )

        Pause ->
            ( { model | runningState = Paused }, Cmd.none )

        Run ->
            ( { model | runningState = Running }, Cmd.none )

        ProcessMessages arcMessages ->
            ( model, Cmd.none )
                |> sequence update arcMessages

        UpdateNumValue key value ->
            let
                newdb =
                    updateData model key (F value)
            in
            ( { model | data = newdb }, Cmd.none )

        UpdateStringValue key value ->
            let
                newdb =
                    updateData model key (S value)
            in
            ( { model | data = newdb }, Cmd.none )

        DeltaNumValueByAmount key delta ->
            let
                oldValue =
                    Dict.get key model.data
            in
            case oldValue of
                Just (F val) ->
                    let
                        newdb =
                            updateData model key (F (val + delta))
                    in
                    ( { model | data = newdb }, Cmd.none )

                Just (S val) ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DeltaNumValueByPercent key percentDelta ->
            let
                oldValue =
                    Dict.get key model.data
            in
            case oldValue of
                Just (F val) ->
                    let
                        newdb =
                            updateData model key (F (val + (percentDelta * val)))
                    in
                    ( { model | data = newdb }, Cmd.none )

                Just (S val) ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        HistoryRequest ->
            let
                _ =
                    log "History Request" ""

                _ =
                    log "Elapsed Time:  " model.elapsedSimTime

                _ =
                    log "Tim in Node:   " model.timeInCurrentNode
            in
            handleTrigger model HistoryRequest

        ExaminationRequest ->
            let
                _ =
                    log "Examination Request" ""

                _ =
                    log "Elapsed Time:  " model.elapsedSimTime

                _ =
                    log "Tim in Node:   " model.timeInCurrentNode
            in
            handleTrigger model ExaminationRequest

        IVFluids rate fluidType ->
            let
                _ =
                    log "IV Fluids rate/type:  " (String.fromInt rate ++ "/" ++ fluidType)

                _ =
                    log "Elapsed Time:         " model.elapsedSimTime

                _ =
                    log "Tim in Node:          " model.timeInCurrentNode
            in
            handleTrigger model (IVFluids rate fluidType)

        O2Therapy fio2 ->
            let
                _ =
                    log "O2 Therapy:   " fio2

                _ =
                    log "Elapsed Time: " model.elapsedSimTime

                _ =
                    log "Tim in Node:  " model.timeInCurrentNode
            in
            handleTrigger model (O2Therapy fio2)

        _ ->
            let
                _ =
                    log "message not matched" msg
            in
            ( model, Cmd.none )


updateSimTime : Model -> Model
updateSimTime model =
    { model | elapsedSimTime = model.elapsedSimTime + 1 }


updateNodeTime : Model -> Model
updateNodeTime model =
    { model | timeInCurrentNode = model.timeInCurrentNode + 1 }


checkArcMatch arc request =
    let
        (Arc _ r _ _) =
            arc
    in
    r == Just request


getMatchingArcs arcs request =
    List.filter (\arc -> checkArcMatch arc request) arcs


handleTrigger model request =
    let
        arcs =
            getMatchingArcs model.currentNode.arcs request

        newModel =
            case arcs of
                [] ->
                    model

                [ arc ] ->
                    eventTransition model arc

                arc :: rest ->
                    -- AGAIN NEVER HAVE > 1 ARC AS A MATCH!!
                    let
                        _ =
                            log "multiple matching arcs" (arc :: rest)
                    in
                    model
    in
    ( newModel, Cmd.none )


eventTransition : Model -> Arc -> Model
eventTransition model arc =
    -- model
    let
        (Arc name _ arcMessages destinationThunk) =
            arc

        ( newModel, newCommands ) =
            update (ProcessMessages arcMessages) model
    in
    { newModel
        | currentNode = destinationThunk ()
        , timeInCurrentNode = 0
    }


maybeTimeoutNode : Model -> Model
maybeTimeoutNode model =
    let
        cn =
            model.currentNode

        newModel =
            case cn.timeout of
                Just t ->
                    if model.timeInCurrentNode >= t then
                        timeoutTransition model

                    else
                        model

                Nothing ->
                    model
    in
    newModel



-- maybe we let this take an arc as a parameter, so we can use for timeout and non-timeout arcs?


timeoutTransition : Model -> Model
timeoutTransition model =
    let
        arc =
            model.currentNode.timeoutArc
    in
    case arc of
        Just (Arc name triggerMessages arcMessages destinationThunk) ->
            let
                ( newModel, newCmds ) =
                    update (ProcessMessages arcMessages) model

                newCurrentTimeInNode =
                    if destinationThunk () == model.currentNode then
                        model.timeInCurrentNode

                    else
                        0
            in
            { newModel
                | currentNode = destinationThunk ()
                , timeInCurrentNode = newCurrentTimeInNode
            }

        Nothing ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ table [ class "pure-table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Parameter" ]
                    , th [] [ text "Value" ]
                    ]
                ]
            , tr []
                [ td [] [ text "Elapsed Sim Time" ]
                , td [] [ text (String.fromInt model.elapsedSimTime) ]
                ]
            , tr []
                [ td [] [ text "Time in Current Node" ]
                , td [] [ text (String.fromInt model.timeInCurrentNode) ]
                ]
            , tr []
                [ td [] [ text "Current Node" ]
                , td [] [ text model.currentNode.name ]
                ]
            ]
        , br [] []
        , dataView model
        , hr [] []
        , div [] [ controlView model ]

        -- , audioView model
        ]


simpleView label value =
    div []
        [ text (label ++ ":   ")
        , text (toString value)
        ]


audioView model =
    div [ id "audio" ]
        [ audio
            [ id "pulse-beep"

            -- src can be a local file too.
            , src "beep.mp3"

            --, src "https://soundbible.com/mp3/Tyrannosaurus%20Rex%20Roar-SoundBible.com-807702404.mp3"
            , controls False
            ]
            []
        ]


makeTableEntry key data =
    let
        row =
            case Dict.get key data of
                Just (F v) ->
                    tr [] [ td [] [ text key ], td [] [ text (String.fromFloat v) ] ]

                Just (S v) ->
                    tr [] [ td [] [ text key ], td [] [ text v ] ]

                Nothing ->
                    tr [] []
    in
    row


dataView : Model -> Html Msg
dataView model =
    let
        data =
            model.data

        keys =
            Dict.keys data
    in
    table [ class "pure-table" ] <|
        [ thead []
            [ tr []
                [ th [] [ text "Parameter" ]
                , th [] [ text "Value" ]
                ]
            ]
        ]
            ++ List.map (\key -> makeTableEntry key data) keys


controlView model =
    div []
        [ button [ class "pure-button", onClick HistoryRequest ] [ text "Request History" ]
        , button [ class "pure-button", onClick ExaminationRequest ] [ text "Perform Examination" ]
        , button [ class "pure-button", onClick (O2Therapy 0.3) ] [ text "O2 Therapy" ]
        , fluidsView model
        , hr [] []
        , runningStateView model
        ]


fluidsView model =
    if model.currentNode.name == "N5" then
        div [] [ text "No fluids left!" ]

    else
        div []
            [ button [ class "pure-button", onClick (IVFluids 100 "saline") ] [ text "IV fluids - 100ml/hr NS" ]
            , button [ class "pure-button", onClick (IVFluids 500 "saline") ] [ text "IV fluids - 500ml/hr NS" ]
            , button [ class "pure-button", onClick (IVFluids 100 "5% Dx") ] [ text "IV fluids - 100ml/hr 5% Dx" ]
            ]


runningStateView model =
    if model.runningState == Running then
        div [] [ button [ class "pure-button", onClick Pause ] [ text "Pause" ] ]

    else
        div [] [ button [ class "pure-button", onClick Run ] [ text "Run" ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        maybehr =
            Dict.get "hr" model.data

        hrPeriod =
            case maybehr of
                Just (F rate) ->
                    60 / rate

                Just (S value) ->
                    1

                Nothing ->
                    1
    in
    Sub.batch
        [ Time.every (1000 / model.speedUp) Tick

        -- This is how we do the pulse oximetry beeping at the HR
        , Time.every (1000 * hrPeriod) OximetryBeep
        ]
