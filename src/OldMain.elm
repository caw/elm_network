module Main exposing (Model, init, initialModel, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time



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
    = ArcMessage ArcMsg
    | UserMessage UserMsg
    | Tick Time.Posix


type ArcMsg
    = Log String
    | Narrate String
    | MoveTo Node


type UserMsg
    = History
    | Examine String
    | Investigation String
    | Intervention String


type Arc
    = Arc String String (List ArcMsg) Node


type alias Node =
    { stringName : String
    , timeout : Int
    , timeoutArc : Arc
    , tirggerArcs : List Arc
    }


type alias Model =
    { elapsedTime : Float
    , speedUp : Float
    }


initialModel =
    { elapsedTime = 0
    , speedUp = 1
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


node1 =
    { stringName = "node1"
    , timout = 1
    , timeoutArc = arc1
    , triggerArcs = [ arc1 ]
    }


arc1 =
    Arc "arc1" "exam" [ Log "Foo", Narrate "Baz" ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text (String.fromFloat model.elapsedTime) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1000 / model.speedUp) Tick
