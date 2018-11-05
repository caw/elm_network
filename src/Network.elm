module Network exposing (n2, n3, n4, n5)

import Types exposing (Arc(..), Msg(..), Node)


n2 : Node
n2 =
    Node "N2"
        (Just (Arc "n2_timeout" Nothing [ UpdateNumValue "saO2" 75, UpdateNumValue "bp" 70, UpdateStringValue "ecg" "slowing AF @ 80", UpdateNumValue "hr" 80 ] (\() -> n4)))
        [ Arc "n1_o2" (Just (O2Therapy 0.3)) [ UpdateNumValue "saO2" 89, UpdateStringValue "ecg" "SR 120", UpdateNumValue "hr" 120 ] (\() -> n3) ]
        (Just 55)


n3 : Node
n3 =
    Node
        "N3"
        (Just (Arc "n3_timeout" Nothing [ UpdateNumValue "saO2" 85, UpdateStringValue "ecg" "AF at 120", UpdateNumValue "hr" 120 ] (\() -> n2)))
        [ Arc "n3_fluids" (Just (IVFluids 100 "saline")) [ UpdateNumValue "bp" 95, UpdateStringValue "ecg" "SR at 90", UpdateNumValue "hr" 90 ] (\() -> n5) ]
        (Just 90)


n4 : Node
n4 =
    Node "N4"
        Nothing
        []
        Nothing


n5 : Node
n5 =
    Node "N5"
        Nothing
        []
        Nothing
