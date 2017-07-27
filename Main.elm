module MetaX exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (href, class, classList, id, src, disabled)
import Html.Events exposing (..)
import Navigation exposing (..)
import Dict exposing (Dict)
import Regex exposing (regex, contains, replace, Regex)
import Json.Encode as Encode
import Json.Helpers as Json
import Json.Decode exposing (int, string, bool, list, nullable, succeed, decodeValue, decodeString, dict, Decoder, Value)
import Json.Decode.Pipeline exposing (decode, required, optional)

import Http

import Ports

main : Program Value Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Card =
    { id : String
    , title : String
    , card_type : String
    , trait : String
    , mp : Int
    , effect : String
    , strength : Maybe Int
    , intelligence : Maybe Int
    , special : Maybe Int
    , image_url : String
    , is_final : Bool
    }

type alias Cards =
    List Card

type alias Deck =
    Dict String Int

type alias Model =
    { location : Location
    , cards : Cards
    , deck : Deck
    }


type Msg
    = NavigateTo String
    | UrlChange Location
    -- | LoadCards
    | CardsLoaded (Result Http.Error Cards)
    | Decrement String
    | Increment String

cardDecoder : Decoder Card
cardDecoder =
    decode Card
        |> required "id" string
        |> required "title" string
        |> required "card_type" string
        |> required "trait" string
        |> required "mp" int
        |> required "effect" string
        |> required "strength" (nullable int)
        |> required "intelligence" (nullable int)
        |> required "special" (nullable int)
        |> required "image_url" string
        |> required "is_final" bool

decoder : Decoder Cards
decoder =
    list cardDecoder

getCards : Cmd Msg
getCards =
    let
        request =
            Http.get "/data/metax.normalized.json" decoder
    in
        Http.send CardsLoaded request

isFinal : Card -> Bool
isFinal card =
    card.is_final == True

-- TODO: Remove hack
filterCards : Cards -> Cards
filterCards cards =
    List.filter isFinal cards

maybeIncrement : String -> Deck -> Deck
maybeIncrement cardId deck =
    Dict.update cardId increment deck

increment : Maybe Int -> Maybe Int
increment value =
    case value of
        Just value ->
            let
                val = if value < 3 then value + 1 else value
            in
                Just val
        Nothing ->
            Just 1

maybeDecrement : String -> Deck -> Deck
maybeDecrement cardId deck =
    Dict.update cardId decrement deck

decrement : Maybe Int -> Maybe Int
decrement value =
    case value of
        Just value ->
            let
                val = if value > 0 then value - 1 else value
            in
                Just val
        Nothing ->
            Just 0

saveDeck : Deck -> Cmd Msg
saveDeck deck =
    Json.encodeMap Encode.string Encode.int deck
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                test =
                    Debug.log "location" location
            in
                ( { model | location = location }, Cmd.none )

        NavigateTo pathname ->
            ( model, newUrl pathname )

        -- LoadCards ->
        --     ( model, fetch )

        CardsLoaded (Ok cards) ->
            let
                -- TODO: Remove hack around unfinalized cards
                finalCards = filterCards cards
            in
                ( { model | cards = finalCards }, Cmd.none )

        CardsLoaded (Err err) ->
            let
                test = Debug.log "err" err
            in
                ( model, Cmd.none )

        Increment cardId ->
            let
                deck = maybeIncrement cardId model.deck
            in
                ( { model | deck = deck }, saveDeck deck )

        Decrement cardId ->
            let
                deck = maybeDecrement cardId model.deck
            in
                ( { model | deck = deck }, Cmd.none )


view : Model -> Html Msg
view model =
    applicationShell model

linkTo : String -> List (Html Msg) -> Html Msg
linkTo pathname =
    a [ class "navitem"
      , href pathname
      , onNavigate (NavigateTo pathname)
      ]

logo : String -> Html Msg
logo title =
    div [ class "navitem logo" ]
        [ text title ]

navbarTop : Model -> Html Msg
navbarTop model =
    nav [ class "navbar-top" ]
        [ logo "MetaX DB"
        -- , linkTo "/" [ text "Cards" ]
        -- , linkTo "/deck" [ text "Deck" ]
        ]

navbarBottom : Model -> Html Msg
navbarBottom model =
    nav [ class "navbar-bottom" ]
        [ linkTo "/" [ text "Cards" ]
        , linkTo "/deck" [ text "Deck" ]
        ]

cardListPane : Model -> Html Msg
cardListPane model =
    div [ id "card-list-pane"
        , class "pane"
        ]
        (List.map (cardView model) model.cards)

isPositive : Int -> Bool
isPositive num =
    num >= 0

stepper : Card -> Deck -> Html Msg
stepper card deck =
    let
        count = Maybe.withDefault 0 (Dict.get card.id deck)

        decrementDisabled = (count == 0)

        incrementDisabled = (count == 3)
    in
        div [ class "stepper-container"]
            [ div [ class "count-container" ] [ text ("In Deck: " ++ toString count) ]
            , button [ class "stepper-button stepper-decrement ripple", disabled decrementDisabled, onClick (Decrement card.id) ] [ text "-" ]
            , button [ class "stepper-button stepper-increment ripple", disabled incrementDisabled, onClick (Increment card.id) ] [ text "+" ]
            ]

mpView : Int -> Html Msg
mpView stat =
    let
        prefix = if stat >= 0 then "+" else ""
    in
        div [ class "card-stat-mp" ] [ text ("MP" ++ ": " ++ prefix ++ toString stat) ]

statView : String -> Maybe Int -> Html Msg
statView label stat =
    case stat of
        Maybe.Just stat ->
            div []
                [ img [ src ("/icons/" ++ label ++ ".png") ] []
                , span [ class "stat" ] [ text (toString stat) ]
                ]
        Maybe.Nothing ->
            text ""

-- TODO: There HAS to be a better way to do this
cardEffect : String -> Html Msg
cardEffect effect =
    let
        hasPlay = contains (regex "PLAY") effect
        hasPush = contains (regex "PUSH") effect
        hasConstant = contains (regex "CONSTANT") effect
        hasAttack = contains (regex "ATTACK") effect
        hasDefend = contains (regex "DEFEND") effect

        scrubbedEffect = replace Regex.All (regex "PLAY|PUSH|CONSTANT|ATTACK|DEFEND") (\_ -> "") effect
    in
        if hasPlay then
            div [ class "card-effect" ]
                [ img [ src "/icons/play.png" ] []
                , text scrubbedEffect
                ]
        else if hasPush then
            div [ class "card-effect" ]
                [ img [ src "/icons/push.png" ] []
                , text scrubbedEffect
                ]
        else if hasConstant then
            div [ class "card-effect" ]
                [ img [ class "upscale", src "/icons/constant.png" ] []
                , text scrubbedEffect
                ]
        else if hasAttack then
            div [ class "card-effect" ]
                [ img [ src "/icons/attack.png" ] []
                , text scrubbedEffect
                ]
        else if hasDefend then
            div [ class "card-effect" ]
                [ img [ src "/icons/defend.png" ] []
                , text scrubbedEffect
                ]
        else
            div [ class "card-effect" ]
                [ text scrubbedEffect ]

cardDetails : Card -> Html Msg
cardDetails card =
    div [ class "card-details" ]
        [ img [ class "card-thumbnail", src card.image_url ] []
        , div [ class "card-text" ]
                [ div [ class "card-title" ] [ text card.title ]
                , cardEffect card.effect
                ]
        , div [ class "card-stats" ]
            [ mpView card.mp
            , statView "strength" card.strength
            , statView "intelligence" card.intelligence
            , statView "special" card.special
            ]
        ]

cardView : Model -> Card -> Html Msg
cardView model card =
    div [ id card.id
        , class "list-item"
        ]
        [ cardDetails card
        , stepper card model.deck
        ]

deckListPane : Model -> Html Msg
deckListPane model =
    div [ id "deck-list-pane"
        , class "pane"
        ]
        [ text "Deck List" ]

paneContainer : Model -> Html Msg
paneContainer model =
    div [ classList [ ( "pane-container", True ), ( "is-deck", model.location.pathname == "/deck" ) ] ]
        [ cardListPane model
        , deckListPane model
        ]

applicationShell : Model -> Html Msg
applicationShell model =
    div [ class "pane-root" ]
        [ navbarTop model
        , paneContainer model
        , navbarBottom model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

deckDecoder : Value -> Deck
deckDecoder session =
    session
        |> decodeValue string
        |> Result.andThen (decodeString (dict int))
        |> Result.withDefault Dict.empty

init : Value -> Location -> ( Model, Cmd Msg )
init session location =
    ( { location = location
      , cards = []
      , deck = deckDecoder session
      },
      getCards
    )


onNavigate : Msg -> Attribute Msg
onNavigate msg =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (succeed msg)
