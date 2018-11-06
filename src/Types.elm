module Types exposing (Arc(..), ArcMessages, ArcName, DbValue(..), FiO2, FluidRate, FluidType, Key, Model, Msg(..), Node, NodeName, RunningState(..), TriggerMsg(..))

import Dict exposing (Dict)
import Time exposing (Posix)


type Msg
    = Tick Time.Posix
    | Tock
    | OximetryBeep Time.Posix
    | UpdateStringValue Key String
    | UpdateNumValue Key Float
    | DeltaNumValueByAmount Key Float
    | DeltaNumValueByPercent Key Float
    | HistoryRequest
    | ExaminationRequest
    | IVFluids FluidRate FluidType
    | O2Therapy FiO2
    | Timeout
    | Trigger TriggerMsg
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


type alias Trigger =
    Msg


type Arc
    = Arc String Trigger (List Msg) (() -> Node)


type alias Node =
    { name : NodeName
    , arcs : List Arc
    , timeout : Maybe Int
    }


type RunningState
    = NotStarted
    | Running
    | Paused


type alias Model =
    { runningState : RunningState
    , elapsedSimTime : Int
    , timeInCurrentNode : Int
    , speedUp : Float
    , currentNode : Node
    , nodes : List Node
    , data : Dict String DbValue
    }
