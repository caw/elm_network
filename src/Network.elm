module Network exposing (n0, n1, n2, n3, n4, n5)

import Types exposing (..)


n0 : Node
n0 =
    Node "START"
        [ Arc "n0_timeout" Timeout [] (\() -> n1) ]
        (Just 0)


n1 : Node
n1 =
    Node "N1"
        [ Arc "n1_timeout" Timeout [ UpdateNumValue "saO2" 75, UpdateStringValue "ecg" "AF", UpdateNumValue "hr" 150 ] (\() -> n2)
        , Arc "ni_tock" Tock [ DeltaNumValueByAmount "bp" -2, DeltaNumValueByPercent "saO2" -1 ] (\() -> n1)
        , Arc "n1_hist" HistoryRequest [ UpdateNumValue "saO2" 80, UpdateStringValue "ecg" "AF", UpdateNumValue "hr" 150 ] (\() -> n2)
        , Arc "n1_exam" ExaminationRequest [ UpdateNumValue "saO2" 80, UpdateStringValue "ecg" "AF", UpdateNumValue "hr" 150 ] (\() -> n2)
        , Arc "n1_o2" (O2Therapy 0.3) [ UpdateNumValue "saO2" 89, UpdateStringValue "ecg" "SR", UpdateNumValue "hr" 90 ] (\() -> n3)
        , Arc "sat < 75" (SimpleDBNumQuery "saO2" LessThan 75) [ UpdateStringValue "ecg" "AF", UpdateNumValue "hr" 150 ] (\() -> n6)
        ]
        (Just 30)


n2 : Node
n2 =
    Node "N2"
        [ Arc "n2_timeout" Timeout [ UpdateNumValue "saO2" 75, UpdateNumValue "bp" 70, UpdateStringValue "ecg" "AF", UpdateNumValue "hr" 140 ] (\() -> n4)
        , Arc "n2_o2" (O2Therapy 0.3) [ UpdateNumValue "saO2" 89, UpdateStringValue "ecg" "SR", UpdateNumValue "hr" 110 ] (\() -> n3)
        ]
        (Just 20)


n3 : Node
n3 =
    Node
        "N3"
        [ Arc "n3_timeout" Timeout [ UpdateNumValue "saO2" 85, UpdateStringValue "ecg" "AF", UpdateNumValue "hr" 120 ] (\() -> n2)
        , Arc "n3_fluids" (IVFluids 100 "saline") [ UpdateNumValue "bp" 95, UpdateStringValue "ecg" "SR", UpdateNumValue "hr" 80 ] (\() -> n5)
        , Arc "n3_tock" Tock [ DeltaNumValueByPercent "saO2" 0.1 ] (\() -> n3)
        ]
        (Just 40)


n4 : Node
n4 =
    Node "N4"
        []
        Nothing


n5 : Node
n5 =
    Node "N5"
        []
        Nothing


n6 : Node
n6 =
    Node "N6"
        []
        Nothing
