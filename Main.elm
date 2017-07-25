module Hello exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (..)
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { text : String
    , location : Location
    }


type Msg
    = Msg1
    | Msg2
    | NavigateTo String
    | UrlChange Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        Msg2 ->
            ( model, Cmd.none )

        UrlChange location ->
            let
                test =
                    Debug.log "location" location
            in
                ( { model | location = location }, Cmd.none )

        NavigateTo pathname ->
            ( model, newUrl pathname )


view : Model -> Html Msg
view model =
    applicationShell model


viewSample : Model -> Html Msg
viewSample model =
    div []
        [ text model.text
        , text " "
        , text model.location.pathname
        , a
            [ href "/test"
            , onNavigate (NavigateTo "/test")
            ]
            [ text "Test" ]
        ]


applicationShell : Model -> Html Msg
applicationShell model =
    div [ class "pane-root" ]
        [ nav [ class "navbar" ]
            [ a
                [ class "navitem"
                , href "/"
                , onNavigate (NavigateTo "/")
                ]
                [ text "Cards" ]
            , a
                [ class "navitem"
                , href "/deck"
                , onNavigate (NavigateTo "/deck")
                ]
                [ text "Deck" ]
            ]
        , div [ class "pane-container" ]
            [ div
                [ id "card-list-pane"
                , class "pane"
                ]
                []
            , div
                [ id "deck-list-pane"
                , class "pane"
                ]
                []
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : Location -> ( Model, Cmd Msg )
init location =
    ( Model "hello" location, Cmd.none )


onNavigate : Msg -> Attribute Msg
onNavigate msg =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (Decode.succeed msg)
