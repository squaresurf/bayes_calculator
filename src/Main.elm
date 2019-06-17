module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events



---- MODEL ----


type alias Model =
    { a : String
    , notA : String
    , b : String
    , aVal : String
    , bVal : String
    , bGivenA : String
    , bGivenNotA : String
    }


init : ( Model, Cmd Msg )
init =
    ( { a = "A"
      , notA = "Not A"
      , b = "B"
      , aVal = "0.1"
      , bVal = "0.1"
      , bGivenA = "0.1"
      , bGivenNotA = "0.1222"
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ChangeA String
    | ChangeNotA String
    | ChangeB String
    | ChangeAVal String
    | ChangeBVal String
    | ChangeBGivenA String
    | ChangeBGivenNotA String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeA newA ->
            ( { model | a = newA }, Cmd.none )

        ChangeNotA newA ->
            ( { model | notA = newA }, Cmd.none )

        ChangeB newB ->
            ( { model | b = newB }, Cmd.none )

        ChangeAVal newAVal ->
            ( { model | aVal = newAVal }, Cmd.none )

        ChangeBVal newBVal ->
            ( { model | bVal = newBVal }, Cmd.none )

        ChangeBGivenA newBGivenA ->
            let
                newModel =
                    { model | bGivenA = newBGivenA }
            in
            ( { newModel | bVal = calculateB newModel }, Cmd.none )

        ChangeBGivenNotA newBGivenNotA ->
            let
                newModel =
                    { model | bGivenNotA = newBGivenNotA }
            in
            ( { newModel | bVal = calculateB newModel }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div [ Attr.style "width" "600px", Attr.style "margin" "0 auto" ]
        [ equationForm model
        , equation model
        , solution model
        ]


equationForm : Model -> Html Msg
equationForm { a, b, notA, aVal, bVal, bGivenA, bGivenNotA } =
    Html.form []
        [ Html.fieldset []
            [ Html.label [ Attr.for "a" ] [ Html.text "A:" ]
            , Html.input [ Attr.id "a", Attr.value a, Events.onInput ChangeA ] []
            ]
        , Html.fieldset []
            [ Html.label [ Attr.for "notA" ] [ Html.text "Not A:" ]
            , Html.input [ Attr.id "notA", Attr.value notA, Events.onInput ChangeNotA ] []
            ]
        , Html.fieldset []
            [ Html.label [ Attr.for "b" ] [ Html.text "B:" ]
            , Html.input [ Attr.id "b", Attr.value b, Events.onInput ChangeB ] []
            ]
        , Html.fieldset []
            [ Html.label [ Attr.for "aVal" ] [ Html.text <| String.concat [ prob a, ":" ] ]
            , Html.input [ Attr.id "aVal", Attr.value aVal, Events.onInput ChangeAVal ] []
            ]
        , Html.fieldset []
            [ Html.label [ Attr.for "bVal" ] [ Html.text <| String.concat [ prob b, ":" ] ]
            , Html.input
                [ Attr.id "bVal"
                , Attr.value bVal
                , Attr.disabled True
                , Attr.style "color" "blue"
                , Events.onInput ChangeBVal
                ]
                []
            ]
        , Html.fieldset []
            [ Html.label [ Attr.for "bGivenA" ] [ Html.text <| String.concat [ pGivenP b a, ":" ] ]
            , Html.input [ Attr.id "bGivenA", Attr.value bGivenA, Events.onInput ChangeBGivenA ] []
            ]
        , Html.fieldset []
            [ Html.label [ Attr.for "bGivenNotA" ] [ Html.text <| String.concat [ pGivenP b notA, ":" ] ]
            , Html.input [ Attr.id "bGivenNotA", Attr.value bGivenNotA, Events.onInput ChangeBGivenNotA ] []
            ]
        ]


equation : Model -> Html Msg
equation { a, b } =
    Html.div []
        [ Html.h2 [] [ Html.text "Equation" ]
        , Html.table []
            [ Html.tbody []
                [ Html.tr []
                    [ Html.td [ Attr.rowspan 2 ] [ Html.text <| String.concat [ pGivenP a b, " = " ] ]
                    , Html.td [ Attr.style "border-bottom" "solid 1px" ]
                        [ Html.text <|
                            String.concat
                                [ pGivenP b a
                                , prob a
                                ]
                        ]
                    ]
                , Html.tr []
                    [ Html.td [] [ Html.text <| prob b ]
                    ]
                ]
            ]
        ]


solution : Model -> Html Msg
solution { a, b, aVal, bVal, bGivenA } =
    case ( String.toFloat aVal, String.toFloat bVal, String.toFloat bGivenA ) of
        ( Just aFloat, Just bFloat, Just bGivenAFloat ) ->
            Html.div []
                [ Html.h2 [] [ Html.text "Solution" ]
                , Html.table []
                    [ Html.tbody []
                        [ Html.tr []
                            [ Html.td [ Attr.rowspan 2 ] [ Html.text <| String.concat [ pGivenP a b, " = " ] ]
                            , Html.td [ Attr.style "border-bottom" "solid 1px" ]
                                [ Html.text <|
                                    String.concat
                                        [ prob bGivenA
                                        , prob aVal
                                        ]
                                ]
                            , Html.td [ Attr.rowspan 2 ]
                                [ Html.text <|
                                    String.concat
                                        [ " = "
                                        , calculateSolution aFloat bFloat bGivenAFloat
                                        ]
                                ]
                            ]
                        , Html.tr []
                            [ Html.td [] [ Html.text <| prob bVal ]
                            ]
                        ]
                    ]
                ]

        _ ->
            Html.p [] [ Html.text "One of your values are incorrect." ]


prob : String -> String
prob p =
    String.concat [ "P(", p, ")" ]


pGivenP : String -> String -> String
pGivenP p1 p2 =
    String.concat [ p1, " | ", p2 ] |> prob



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- General ----


calculateSolution : Float -> Float -> Float -> String
calculateSolution a b bGivenA =
    (bGivenA * a) / b |> String.fromFloat


calculateB : Model -> String
calculateB { aVal, bVal, bGivenA, bGivenNotA } =
    case ( String.toFloat aVal, String.toFloat bGivenA, String.toFloat bGivenNotA ) of
        ( Just aFloat, Just bGivenAFloat, Just bGivenNotAFloat ) ->
            (bGivenAFloat * aFloat) + (bGivenNotAFloat * (1 - aFloat)) |> String.fromFloat

        _ ->
            bVal
