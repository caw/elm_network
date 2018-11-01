module Main exposing (Model, init, initialModel, main, subscriptions, update, view)

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
    | Trigger TriggerMsg
    | ProcessMessages ArcMessages


type TriggerMsg
    = History
    | Examination


type DbValue
    = F Float
    | S String


type alias Key =
    String


type alias ArcName =
    String


type alias ArcMessages =
    List Msg


type alias NodeName =
    String


type Arc
    = Arc String TriggerMsg (List Msg) (() -> Node)


type alias Node =
    { name : NodeName
    , timeoutArc : Maybe Arc
    , arcs : List Arc
    , timeout : Maybe Int
    }


initialData : Dict.Dict String DbValue
initialData =
    Dict.fromList
        [ ( "bp", F 100.0 )
        , ( "temp", F 37.0 )
        , ( "hr", F 65.0 )
        , ( "ecg", S "Normal" )
        ]


n1 =
    Node "n1" (Just a3) [ a1, a2 ] (Just 5)


n2 =
    Node "n2" Nothing [ a2 ] (Just 1)


a1 : Arc
a1 =
    Arc "a1" History [ UpdateNumValue "hr" 333, UpdateStringValue "ecg" "WFT!!!!" ] (\() -> n2)


a2 : Arc
a2 =
    Arc "a2" Examination [ UpdateNumValue "bp" 101, UpdateNumValue "temp" 37.0000001 ] (\() -> n1)


a3 : Arc
a3 =
    Arc "a3" History [ UpdateNumValue "bp" 1000000, UpdateStringValue "ecg" "NFI" ] (\() -> n2)



-- Arc "a3" History [ UpdateNumValue "bp" 1000000 ] (\() -> n2)


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
    let
        _ =
            log key
    in
    Dict.update key (\_ -> Just value) model.data



-- get key db
-- is already in Dict
-- UPDATE


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
            let
                _ =
                    log "messages" (toString arcMessages)
            in
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


maybeTimeoutNode : Model -> Model
maybeTimeoutNode model =
    let
        cn =
            model.currentNode

        newModel =
            case cn.timeout of
                Just t ->
                    if model.timeInCurrentNode >= t then
                        let
                            m =
                                timeoutNode model

                            _ =
                                log "m.db:  " m.data
                        in
                        m

                    else
                        model

                Nothing ->
                    model
    in
    newModel


timeoutNode : Model -> Model
timeoutNode model =
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
            { newModel | currentNode = destinationThunk () }

        Nothing ->
            model



{- ( new_current_node, new_model, new_commands ) =
           case arc of
               Just (Arc name triggerMessages arcMessages destinationThunk) ->
                   let
                       ( newMdl, newCmds ) =
                           update (ProcessMessages arcMessages) model
                   in
                   ( destinationThunk (), newMdl, newCmds )

               Nothing ->
                   -- **************** ERROR ******************** no timeoutArc - need to make this imposible state impossible at compile time
                   let
                       _ =
                           log "NOTHING !!!!!"
                   in
                   ( model.currentNode, model, Cmd.none )
   in
   { new_model | currentNode = new_current_node }

-}
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "elapsedSimTime:  "
        , text (String.fromInt model.elapsedSimTime ++ "   ")
        , text "timeInCurrentNode: "
        , text (String.fromInt model.timeInCurrentNode ++ "   ")
        , text model.currentNode.name
        , div [] [ text (toString model.data) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1000 / model.speedUp) Tick
