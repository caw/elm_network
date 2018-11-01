module Main exposing (Model, Msg(..), getMatchingArcs, init, initialModel, main, subscriptions, update, view)

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
    | FluidsRequest
    | O2Therapy FiO2
    | Trigger TriggerMsg
    | IVFluids FluidType FluidRate
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
        , ( "saO2", F 120 )
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
        []
        (Just 55)


n3 =
    Node
        "N3"
        (Just (Arc "n3_timeout" Nothing [ UpdateNumValue "saO2" 85, UpdateStringValue "ecg" "AF at 120" ] (\() -> n2)))
        [ Arc "n3_fluids" (Just (IVFluids "saline" 200)) [ UpdateNumValue "bp" 95 ] (\() -> n5) ]
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


a1 : Arc
a1 =
    Arc "a1" (Just HistoryRequest) [ UpdateNumValue "hr" 333, UpdateStringValue "ecg" "WFT!!!!" ] (\() -> n2)


a2 : Arc
a2 =
    Arc "a2" (Just ExaminationRequest) [ UpdateNumValue "bp" 101, UpdateNumValue "temp" 37.0000001 ] (\() -> n1)


a3 : Arc
a3 =
    Arc "a3" Nothing [ UpdateNumValue "bp" 1000000, UpdateStringValue "ecg" "NFI" ] (\() -> n2)


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
            handleUserRequest model HistoryRequest

        ExaminationRequest ->
            handleUserRequest model ExaminationRequest

        FluidsRequest ->
            handleUserRequest model FluidsRequest

        O2Therapy fio2 ->
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
    let
        matches =
            List.filter (\arc -> checkArcMatch arc request) arcs

        _ =
            log "matches with request: " request

        _ =
            log "arcs: " matches
    in
    matches


handleUserRequest model request =
    let
        arcs =
            model.currentNode.arcs

        _ =
            log "arcs" arcs
    in
    ( model, Cmd.none )


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
        , text (toString model.data)
        , hr [] []
        , div
            []
            [ controlView model
            ]
        ]


simpleView label value =
    div []
        [ text (label ++ ":   ")
        , text (toString value)
        ]


controlView model =
    div []
        [ button [ onClick HistoryRequest ] [ text "Request History" ]
        , button [ onClick ExaminationRequest ] [ text "Perform Examination" ]
        , button [ onClick FluidsRequest ] [ text "Give IV fluids" ]
        , button [ onClick (O2Therapy 0.5) ] [ text "O2 Therapy" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1000 / model.speedUp) Tick
