module Main exposing (Model, Msg(..), checkArcMatch, getMatchingArcs, handleUserRequest, init, initialModel, main, n1, n2, n3, n4, n5, subscriptions, update, view)

import Browser
import Debug exposing (log, toString)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Update.Extra exposing (sequence)



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
    | UpdateStringValue Key String
    | UpdateNumValue Key Float
    | HistoryRequest
    | ExaminationRequest
    | IVFluids FluidRate FluidType
    | O2Therapy FiO2
    | Trigger TriggerMsg
    | ProcessMessages ArcMessages


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


initialData : Dict.Dict String DbValue
initialData =
    Dict.fromList
        [ ( "bp", F 85.0 )
        , ( "temp", F 37.0 )
        , ( "hr", F 65.0 )
        , ( "saO2", F 85 )
        , ( "ecg", S "SR 100" )
        ]


n0 =
    Node "START" (Just (Arc "n0_timeout" Nothing [] (\() -> n1))) [] (Just 0)


n1 =
    Node "N1"
        (Just (Arc "n1_timeout" Nothing [ UpdateNumValue "saO2" 75, UpdateStringValue "ecg" "AF at 150" ] (\() -> n2)))
        [ Arc "n1_hist" (Just HistoryRequest) [ UpdateNumValue "saO2" 80, UpdateStringValue "ecg" "AF at 150" ] (\() -> n2)
        , Arc "n1_exam" (Just ExaminationRequest) [ UpdateNumValue "saO2" 80, UpdateStringValue "ecg" "AF at 150" ] (\() -> n2)
        , Arc "n1_o2" (Just (O2Therapy 0.3)) [ UpdateNumValue "saO2" 89, UpdateStringValue "ecg" "SR 120" ] (\() -> n3)
        ]
        (Just 60)


n2 =
    Node "N2"
        (Just (Arc "n2_timeout" Nothing [ UpdateNumValue "saO2" 75, UpdateNumValue "bp" 70, UpdateStringValue "ecg" "slowing AF @ 80" ] (\() -> n4)))
        [ Arc "n1_o2" (Just (O2Therapy 0.3)) [ UpdateNumValue "saO2" 89, UpdateStringValue "ecg" "SR 120" ] (\() -> n3) ]
        (Just 55)


n3 =
    Node
        "N3"
        (Just (Arc "n3_timeout" Nothing [ UpdateNumValue "saO2" 85, UpdateStringValue "ecg" "AF at 120" ] (\() -> n2)))
        [ Arc "n3_fluids" (Just (IVFluids 100 "saline")) [ UpdateNumValue "bp" 95 ] (\() -> n5) ]
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
    { elapsedSimTime : Int
    , timeInCurrentNode : Int
    , speedUp : Float
    , currentNode : Node
    , nodes : List Node
    , data : Dict String DbValue
    }


initialModel : Model
initialModel =
    { elapsedSimTime = 0
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
            model
                |> updateSimTime
                |> updateNodeTime
                |> maybeTimeoutNode
                |> (\newModel -> ( newModel, Cmd.none ))

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

        HistoryRequest ->
            let
                _ =
                    log "History Request" ""

                _ =
                    log "Elapsed Time:  " model.elapsedSimTime

                _ =
                    log "Tim in Node:   " model.timeInCurrentNode
            in
            handleUserRequest model HistoryRequest

        ExaminationRequest ->
            let
                _ =
                    log "Examination Request" ""

                _ =
                    log "Elapsed Time:  " model.elapsedSimTime

                _ =
                    log "Tim in Node:   " model.timeInCurrentNode
            in
            handleUserRequest model ExaminationRequest

        IVFluids rate fluidType ->
            let
                _ =
                    log "IV Fluids rate/type:  " (String.fromInt rate ++ "/" ++ fluidType)

                _ =
                    log "Elapsed Time:         " model.elapsedSimTime

                _ =
                    log "Tim in Node:          " model.timeInCurrentNode
            in
            handleUserRequest model (IVFluids rate fluidType)

        O2Therapy fio2 ->
            let
                _ =
                    log "O2 Therapy:   " fio2

                _ =
                    log "Elapsed Time: " model.elapsedSimTime

                _ =
                    log "Tim in Node:  " model.timeInCurrentNode
            in
            handleUserRequest model (O2Therapy fio2)

        _ ->
            let
                _ =
                    log "message not matched" msg
            in
            ( model, Cmd.none )


checkArcMatch arc request =
    let
        (Arc _ r _ _) =
            arc
    in
    r == Just request


getMatchingArcs arcs request =
    List.filter (\arc -> checkArcMatch arc request) arcs


handleUserRequest model request =
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


updateSimTime : Model -> Model
updateSimTime model =
    { model | elapsedSimTime = model.elapsedSimTime + 1 }


updateNodeTime : Model -> Model
updateNodeTime model =
    { model | timeInCurrentNode = model.timeInCurrentNode + 1 }


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
            in
            { newModel
                | currentNode = destinationThunk ()
                , timeInCurrentNode = 0
            }

        Nothing ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ simpleView "Elapsed Sim Time" model.elapsedSimTime
        , simpleView "Time in Current Node" model.timeInCurrentNode
        , simpleView "Current Node" model.currentNode.name
        , hr [] []
        , dataView model
        , hr [] []
        , div
            []
            [ controlView model
            ]
        ]


dataView : Model -> Html Msg
dataView model =
    text (toString (Dict.toList model.data))


simpleView label value =
    div []
        [ text (label ++ ":   ")
        , text (toString value)
        ]


controlView model =
    div []
        [ button [ onClick HistoryRequest ] [ text "Request History" ]
        , button [ onClick ExaminationRequest ] [ text "Perform Examination" ]
        , button [ onClick (O2Therapy 0.3) ] [ text "O2 Therapy" ]
        , hr [] []
        , button [ onClick (IVFluids 100 "saline") ] [ text "Give IV fluids - 100ml/hr NS" ]
        , button [ onClick (IVFluids 500 "saline") ] [ text "Give IV fluids - 500ml/hr NS" ]
        , button [ onClick (IVFluids 100 "5% Dx") ] [ text "Give IV fluids - 100ml/hr 5% Dx" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1000 / model.speedUp) Tick
