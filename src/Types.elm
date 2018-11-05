module Types exposing (Arc(..), ArcMessages, ArcName, DbValue(..), FiO2, FluidRate, FluidType, Key, Msg(..), Node, NodeName, RunningState(..), TriggerMsg(..))

import Time exposing (Posix)


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
