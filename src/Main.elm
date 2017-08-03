module Main exposing (Model, Msg, update, view, subscriptions, init)

import Http
import Html exposing (..)
import Html.Attributes exposing (href, class, classList, id, src, disabled)
import Html.Events exposing (..)
import Navigation exposing (..)
import Dict exposing (Dict)
import Regex exposing (regex, contains, replace, Regex)
import Json.Decode exposing (Value)
import Data.Card as Card exposing (Card)
import Data.CardList as CardList exposing (CardList)
import Data.Deck as Deck exposing (Deck)
import Request.Deck
import Request.CardList
import Util exposing (onNavigate)

import Compare exposing (concat, by, Comparator)

import Route exposing (fromLocation, Route)


main : Program Value Model Msg
main =
    Navigation.programWithFlags (fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { location : Maybe Route
    , cards : CardList
    , deck : Deck
    , card : Maybe String
    }


type Msg
    = NavigateTo String
    | SetRoute (Maybe Route)
    | CardsLoaded (Result Http.Error CardList)
    | Decrement String
    | Increment String


maybeIncrement : String -> Deck -> Deck
maybeIncrement cardId deck =
    Dict.update cardId increment deck


increment : Maybe Int -> Maybe Int
increment value =
    case value of
        Just value ->
            let
                val =
                    if value < 3 then
                        value + 1
                    else
                        value
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
                val =
                    if value > 0 then
                        value - 1
                    else
                        value
            in
                Just val

        Nothing ->
            Just 0


notZero : String -> Int -> Bool
notZero _ count =
    count /= 0

forcedOrder : Card -> Int
forcedOrder card =
    case card.card_type of
        "Character" -> 1
        "Battle" -> 2
        "Event" -> 3
        _ -> 4

type BattleCardType
    = Strength
    | Intelligence
    | Special
    | Multi
    | None

battleCardType : Card -> BattleCardType
battleCardType card =
    if (contains (regex "^Strength \\d+$") card.title) then Strength
    else if (contains (regex "^Intelligence \\d+$") card.title) then Intelligence
    else if (contains (regex "^Special \\d+$") card.title) then Special
    else if (contains (regex "^(?:(?:Strength|Intelligence|Special)\\/){1,3} \\d+$") card.title) then Multi
    else None

battleOrder : Card -> Int
battleOrder card =
    case battleCardType card of
        Strength -> 1
        Intelligence -> 2
        Special -> 3
        Multi -> 4
        None -> 5

cardListSort : Comparator Card
cardListSort =
    concat [ by forcedOrder, by battleOrder, by .card_type, by .title, by .effect ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute route ->
            case route of
                Just Route.Home ->
                    ( { model | location = route }, Cmd.none )
                Just Route.Deck ->
                    ( { model | location = route }, Cmd.none )
                Just (Route.Card cardId )->
                    ( { model | location = route, card = Just cardId }, Cmd.none )
                Nothing ->
                    ( { model | location = Just Route.Home }, Cmd.none )

        NavigateTo pathname ->
            ( model, newUrl pathname )

        CardsLoaded (Ok cards) ->
            ( { model | cards = (List.sortWith cardListSort cards) }, Cmd.none )

        CardsLoaded (Err err) ->
            let
                test =
                    Debug.log "err" err
            in
                ( model, Cmd.none )

        Increment cardId ->
            let
                deck =
                    maybeIncrement cardId model.deck
            in
                ( { model | deck = deck }, Request.Deck.save deck )

        Decrement cardId ->
            let
                deck =
                    Dict.filter notZero (maybeDecrement cardId model.deck)
            in
                ( { model | deck = deck }, Request.Deck.save deck )


view : Model -> Html Msg
view model =
    applicationShell model


linkTo : String -> List (Html Msg) -> Html Msg
linkTo pathname =
    a
        [ class "navitem"
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
        [ logo "MetaX DB" ]


navbarBottom : Model -> Html Msg
navbarBottom model =
    let
        deckContents =
            (List.map (Tuple.mapFirst (lookup model)) (Dict.toList model.deck))

        deckSize =
            sum deckContents
    in
        nav [ class "navbar-bottom" ]
            [ linkTo "/" [ text "Cards" ]
            , linkTo "/deck" [ text ("Deck (" ++ (toString deckSize) ++ ")") ]
            ]


cardListPane : Model -> Html Msg
cardListPane model =
    div
        [ id "card-list-pane"
        , class "pane"
        ]
        (List.map (cardView model) model.cards)


isPositive : Int -> Bool
isPositive num =
    num >= 0


stepper : ( Card, Int ) -> Html Msg
stepper ( card, count ) =
    let
        decrementDisabled =
            (count == 0)

        incrementDisabled =
            (count == 3)
    in
        div [ class "stepper-container" ]
            [ button [ class "stepper-button stepper-decrement ripple", disabled decrementDisabled, onClick (Decrement card.id) ] [ text "-" ]
            , button [ class "stepper-button stepper-increment ripple", disabled incrementDisabled, onClick (Increment card.id) ] [ text "+" ]
            , div [ class "count-container" ] [ text (toString count) ]
            ]


mpView : Int -> Html Msg
mpView stat =
    let
        prefix =
            if stat >= 0 then
                "+"
            else
                ""
    in
        div [ class "card-stat-mp" ] [ text ("MP" ++ ": " ++ prefix ++ toString stat) ]


statView : String -> Maybe Int -> Html Msg
statView statType stat =
    case stat of
        Maybe.Just stat ->
            div [ class "card-stat" ]
                [ img [ class "card-stat-icon", src ("/icons/" ++ statType ++ ".png") ] []
                , span [ class "card-stat-text" ] [ text (toString stat) ]
                ]

        Maybe.Nothing ->
            text ""



-- TODO: There HAS to be a better way to do this


cardEffect : String -> Html Msg
cardEffect effect =
    let
        hasPlay =
            contains (regex "PLAY") effect

        hasPush =
            contains (regex "PUSH") effect

        hasConstant =
            contains (regex "CONSTANT") effect

        hasAttack =
            contains (regex "ATTACK") effect

        hasDefend =
            contains (regex "DEFEND") effect

        scrubbedEffect =
            replace Regex.All (regex "PLAY|PUSH|CONSTANT|ATTACK|DEFEND") (\_ -> "") effect
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
        [ a [ class "card-thumbnail"
            , onNavigate (NavigateTo ("/card/" ++ card.id))
            ]
            [ img [ src (replace Regex.All (regex "/images/") (\_ -> "/thumbnails/") card.image_url) ] []
            ]
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
    let
        count =
            Maybe.withDefault 0 (Dict.get card.id model.deck)
    in
        div
            [ id card.id
            , class "list-item"
            ]
            [ cardDetails card
            , stepper ( card, count )
            ]


idMatches : String -> Card -> Bool
idMatches cardId card =
    card.id == cardId


lookup : Model -> String -> Maybe Card
lookup model cardId =
    List.filter (idMatches cardId) model.cards
        |> List.head


byType : String -> ( Maybe Card, Int ) -> Bool
byType cardType ( card, _ ) =
    case card of
        Just card ->
            card.card_type == cardType

        Nothing ->
            False


byTitle : String -> ( Maybe Card, Int ) -> Bool
byTitle cardTitle ( card, _ ) =
    case card of
        Just card ->
            card.title == cardTitle

        Nothing ->
            False


byMulti : String -> ( Maybe Card, Int ) -> Bool
byMulti rank ( card, _ ) =
    case card of
        Just card ->
            contains (regex ("(?:(?:Strength|Intelligence|Special)\\/){1,3}\\D+" ++ rank ++ "$")) card.title

        Nothing ->
            False


sectionHeader : String -> Int -> List (Html Msg)
sectionHeader title count =
    List.singleton (div [ class "list-item-header" ] [ text <| title ++ " (" ++ (toString count) ++ ")" ])


sectionSubHeader : String -> Int -> List (Html Msg)
sectionSubHeader title count =
    List.singleton (div [ class "list-item-sub-header" ] [ text <| title ++ " (" ++ (toString count) ++ ")" ])


sum : List ( Maybe Card, Int ) -> Int
sum cards =
    (List.sum (List.map Tuple.second cards))


charactersView : List ( Maybe Card, Int ) -> List (Html Msg)
charactersView characters =
    if List.length characters > 0 then
        List.concat
            [ sectionHeader "Characters" (sum characters)
            , (List.map deckCardView characters)
            ]
    else
        []


eventsView : List ( Maybe Card, Int ) -> List (Html Msg)
eventsView events =
    if List.length events > 0 then
        List.concat
            [ sectionHeader "Events" (sum events)
            , (List.map deckCardView events)
            ]
    else
        []


bcWarningView : List (Html Msg)
bcWarningView =
    List.singleton (div [ class "list-item-warning" ] [ text "You have too many Battle Cards at this Type/Rank" ])


battleCardSubSection : String -> List ( Maybe Card, Int ) -> List (Html Msg)
battleCardSubSection title cards =
    if List.length cards > 0 then
        let
            warning =
                if sum cards > 3 then
                    bcWarningView
                else
                    []
        in
            List.concat
                [ sectionSubHeader title (sum cards)
                , warning
                , (List.map deckCardView cards)
                ]
    else
        []


battleCardView : List ( Maybe Card, Int ) -> List (Html Msg)
battleCardView battle =
    if List.length battle > 0 then
        let
            -- Strength
            str1 =
                Tuple.first <| List.partition (byTitle "Strength 1") battle

            str2 =
                Tuple.first <| List.partition (byTitle "Strength 2") battle

            str3 =
                Tuple.first <| List.partition (byTitle "Strength 3") battle

            str4 =
                Tuple.first <| List.partition (byTitle "Strength 4") battle

            str5 =
                Tuple.first <| List.partition (byTitle "Strength 5") battle

            str6 =
                Tuple.first <| List.partition (byTitle "Strength 6") battle

            str7 =
                Tuple.first <| List.partition (byTitle "Strength 7") battle

            -- Intelligence
            int1 =
                Tuple.first <| List.partition (byTitle "Intelligence 1") battle

            int2 =
                Tuple.first <| List.partition (byTitle "Intelligence 2") battle

            int3 =
                Tuple.first <| List.partition (byTitle "Intelligence 3") battle

            int4 =
                Tuple.first <| List.partition (byTitle "Intelligence 4") battle

            int5 =
                Tuple.first <| List.partition (byTitle "Intelligence 5") battle

            int6 =
                Tuple.first <| List.partition (byTitle "Intelligence 6") battle

            int7 =
                Tuple.first <| List.partition (byTitle "Intelligence 7") battle

            -- Special
            sp1 =
                Tuple.first <| List.partition (byTitle "Special 1") battle

            sp2 =
                Tuple.first <| List.partition (byTitle "Special 2") battle

            sp3 =
                Tuple.first <| List.partition (byTitle "Special 3") battle

            sp4 =
                Tuple.first <| List.partition (byTitle "Special 4") battle

            sp5 =
                Tuple.first <| List.partition (byTitle "Special 5") battle

            sp6 =
                Tuple.first <| List.partition (byTitle "Special 6") battle

            sp7 =
                Tuple.first <| List.partition (byTitle "Special 7") battle

            -- Multi
            multi1 =
                Tuple.first <| List.partition (byMulti "1") battle

            multi2 =
                Tuple.first <| List.partition (byMulti "2") battle

            multi3 =
                Tuple.first <| List.partition (byMulti "3") battle

            multi4 =
                Tuple.first <| List.partition (byMulti "4") battle

            multi5 =
                Tuple.first <| List.partition (byMulti "5") battle

            multi6 =
                Tuple.first <| List.partition (byMulti "6") battle

            multi7 =
                Tuple.first <| List.partition (byMulti "7") battle
        in
            List.concat
                [ sectionHeader "Battle Cards" (sum battle)

                -- Strength
                , battleCardSubSection "Strength - Rank 1" str1
                , battleCardSubSection "Strength - Rank 2" str2
                , battleCardSubSection "Strength - Rank 3" str3
                , battleCardSubSection "Strength - Rank 4" str4
                , battleCardSubSection "Strength - Rank 5" str5
                , battleCardSubSection "Strength - Rank 6" str6
                , battleCardSubSection "Strength - Rank 7" str7

                -- Intelligence
                , battleCardSubSection "Intelligence - Rank 1" int1
                , battleCardSubSection "Intelligence - Rank 2" int2
                , battleCardSubSection "Intelligence - Rank 3" int3
                , battleCardSubSection "Intelligence - Rank 4" int4
                , battleCardSubSection "Intelligence - Rank 5" int5
                , battleCardSubSection "Intelligence - Rank 6" int6
                , battleCardSubSection "Intelligence - Rank 7" int7

                -- Special
                , battleCardSubSection "Special - Rank 1" sp1
                , battleCardSubSection "Special - Rank 2" sp2
                , battleCardSubSection "Special - Rank 3" sp3
                , battleCardSubSection "Special - Rank 4" sp4
                , battleCardSubSection "Special - Rank 5" sp5
                , battleCardSubSection "Special - Rank 6" sp6
                , battleCardSubSection "Special - Rank 7" sp7

                -- Multi
                , battleCardSubSection "Multi - Rank 1" multi1
                , battleCardSubSection "Multi - Rank 2" multi2
                , battleCardSubSection "Multi - Rank 3" multi3
                , battleCardSubSection "Multi - Rank 4" multi4
                , battleCardSubSection "Multi - Rank 5" multi5
                , battleCardSubSection "Multi - Rank 6" multi6
                , battleCardSubSection "Multi - Rank 7" multi7
                ]
    else
        []


deckSectionView : List ( Maybe Card, Int ) -> List (Html Msg)
deckSectionView cards =
    let
        characters =
            Tuple.first <| List.partition (byType "Character") cards

        events =
            Tuple.first <| List.partition (byType "Event") cards

        battle =
            Tuple.first <| List.partition (byType "Battle") cards
    in
        List.concat
            [ charactersView characters
            , eventsView events
            , battleCardView battle
            ]


deckCardView : ( Maybe Card, Int ) -> Html Msg
deckCardView card =
    case card of
        ( Just card, count ) ->
            div
                [ id ("deck_" ++ card.id)
                , class "list-item"
                ]
                [ div [ class "deck-card-details" ]
                    [ div [ class "card-title" ]
                        [ text ("(" ++ card.id ++ ") ")
                        , text card.title
                        ]
                    , mpView card.mp
                    ]
                , div [ class "deck-card-stats" ]
                    [ statView "strength" card.strength
                    , statView "intelligence" card.intelligence
                    , statView "special" card.special
                    ]
                , stepper ( card, count )
                ]

        ( Nothing, _ ) ->
            div [] []


deckListPane : Model -> Html Msg
deckListPane model =
    div
        [ id "deck-list-pane"
        , class "pane"
        ]
        -- TODO: use |> operator
        (deckSectionView (List.map (Tuple.mapFirst (lookup model)) (Dict.toList model.deck)))

cardPane : String -> Model -> Html Msg
cardPane cardId model =
    let
        card = lookup model cardId
    in
        case card of
            Just card ->
                div [ id "card-pane"
                    , class "pane"
                    ]
                    [ img [ class "card-full"
                          , src card.image_url
                          ]
                          []
                    ]
            Nothing ->
                div [ id "card-pane"
                    , class "pane"
                    ]
                    [ text "Card not found"
                    ]

paneContainer : Model -> Html Msg
paneContainer model =
    case model.location of
        Just Route.Home ->
            div [ class "pane-container" ]
                [ cardListPane model
                , deckListPane model
                ]
        Just Route.Deck ->
            div [ classList [ ( "pane-container", True ), ( "is-deck", True ) ] ]
                [ cardListPane model
                , deckListPane model
                ]
        Just (Route.Card id) ->
            div [ class "pane-container" ]
                [ cardPane id model
                ]
        Nothing ->
            div [ class "pane-container" ]
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


init : Value -> Location -> ( Model, Cmd Msg )
init session location =
    ( { location = fromLocation location
      , cards = []
      , card = Nothing
      -- TODO: avoid loading a deck list before cards are loaded
      , deck = Deck.decoder session
      }
    , Request.CardList.load
        |> Http.send CardsLoaded
    )
