port module Main exposing (handleTrigger, init, initialModel, main, subscriptions, update, view)

import Browser
import Debug exposing (log, toString)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as E
import Network exposing (..)
import Time
import Types exposing (..)
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


initialData : Dict.Dict String DbValue
initialData =
    Dict.fromList
        [ ( "bp", F 85.0 )
        , ( "temp", F 37.0 )
        , ( "hr", F 100.0 )
        , ( "saO2", F 85 )
        , ( "ecg", S "SR" )
        ]


rangeData =
    Dict.fromList
        [ ( "bp", ( 60, 150 ) )
        , ( "temp", ( 34, 42 ) )
        , ( "hr", ( 30, 150 ) )
        , ( "saO2", ( 60, 100 ) )
        ]


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
        Tick _ ->
            let
                ( newModel, newCommands ) =
                    update CheckDBTrigger model
            in
            if newModel.runningState == Running then
                if timedout newModel then
                    -- is the currentNode timed out?
                    handleTrigger
                        { newModel
                            | elapsedSimTime = newModel.elapsedSimTime + 1
                            , timeInCurrentNode = 0
                        }
                        Timeout

                else
                    {- -- this is a "checkDBQueryArc" really...
                       let
                           arcs =
                               -- get a list of DB queries which are true
                               checkDBQueryArcs model
                       in
                       case List.head arcs of
                           Just arc ->
                               let
                                   newModel =
                                       eventTransition model arc
                               in
                               ( { newModel
                                   | elapsedSimTime = model.elapsedSimTime + 1
                                   }
                               , Cmd.none
                               )

                           Nothing ->
                    -}
                    handleTrigger
                        { newModel
                            | elapsedSimTime = newModel.elapsedSimTime + 1
                            , timeInCurrentNode = newModel.timeInCurrentNode + 1
                        }
                        Tock

            else
                ( model, Cmd.none )

        CheckDBTrigger ->
            let
                arcs =
                    -- get a list of DB queries which are true
                    checkDBQueryArcs model
            in
            case List.head arcs of
                Just arc ->
                    let
                        newModel =
                            transition model arc
                    in
                    ( newModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Pause ->
            ( { model | runningState = Paused }, Cmd.none )

        Run ->
            ( { model | runningState = Running }, Cmd.none )

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
                            updateData model key (F (val + ((percentDelta / 100.0) * val)))
                    in
                    ( { model | data = newdb }, Cmd.none )

                Just (S val) ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OximetryBeep _ ->
            if model.runningState == Running then
                ( model, sounds (E.string "beep") )

            else
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


checkDBQueryArcMatch model arc =
    let
        (Arc _ trigger _ _) =
            arc
    in
    case trigger of
        SimpleDBNumQuery key comp value ->
            let
                d =
                    Dict.get key model.data
            in
            case d of
                Just (F v) ->
                    case comp of
                        LessThan ->
                            v < value

                        GreaterThan ->
                            v > value

                Just (S v) ->
                    False

                Nothing ->
                    False

        _ ->
            False


checkDBQueryArcs model =
    List.filter (\arc -> checkDBQueryArcMatch model arc) model.currentNode.arcs


checkTriggerArcMatch arc trigger =
    let
        (Arc _ t _ _) =
            arc
    in
    t == trigger


getMatchingTriggerArcs arcs request =
    List.filter (\arc -> checkTriggerArcMatch arc request) arcs


transition : Model -> Arc -> Model
transition model arc =
    -- model
    let
        (Arc name _ arcMessages destinationThunk) =
            arc

        ( newModel, newCommands ) =
            sequence update arcMessages ( model, Cmd.none )
    in
    if newModel.currentNode == destinationThunk () then
        newModel

    else
        { newModel
            | currentNode = destinationThunk ()
            , timeInCurrentNode = 0
        }


handleTrigger model trigger =
    let
        timeInNode =
            model.timeInCurrentNode

        arcs =
            getMatchingTriggerArcs model.currentNode.arcs trigger

        newModel =
            case arcs of
                [] ->
                    model

                [ arc ] ->
                    transition model arc

                arc :: rest ->
                    -- AGAIN NEVER HAVE > 1 ARC AS A MATCH ****** IT'S DETERMINISTIC *******!!
                    let
                        _ =
                            log "multiple matching arcs" (arc :: rest)
                    in
                    model
    in
    ( newModel, Cmd.none )


timedout : Model -> Bool
timedout model =
    case model.currentNode.timeout of
        Just t ->
            model.timeInCurrentNode >= t

        Nothing ->
            False



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ modelParamView model
        , br [] []
        , dbView model
        , hr [] []
        , logView model
        , hr [] []
        , div [] [ controlView model ]
        ]


modelParamView model =
    table [ class "pure-table" ]
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


logView model =
    textarea [ rows 11, readonly True ] [ text "Hello World" ]


makeDBTableEntry key data =
    let
        row =
            case Dict.get key data of
                Just (F v) ->
                    tr [] [ td [] [ text key ], td [] [ text (String.fromInt (round v)) ] ]

                Just (S v) ->
                    tr [] [ td [] [ text key ], td [] [ text v ] ]

                Nothing ->
                    tr [] []
    in
    row


dbView : Model -> Html Msg
dbView model =
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
            ++ List.map (\key -> makeDBTableEntry key data) keys


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
        --, Time.every (1000 * hrPeriod) OximetryBeep
        ]
