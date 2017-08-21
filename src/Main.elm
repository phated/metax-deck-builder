module Main exposing (Model, Msg, update, view, subscriptions, init)

import Http
import Html exposing (nav, div, img, text, button, a, span, label, input, Html)
import Html.Attributes exposing (href, class, classList, id, src, disabled, type_, checked)
import Html.Events exposing (onClick, onCheck)
import Navigation exposing (newUrl, Location)
import Dict exposing (Dict)
import Regex exposing (regex, contains, replace, Regex)
import Json.Decode exposing (Value)
import Data.Card as Card exposing (Card)
import Data.CardList as CardList exposing (CardList)
import Data.CardType exposing (CardType(..), BattleType(..))
import Data.CardEffect exposing (CardEffect(..), effectToString, effectToHtml)
import Data.CardRarity exposing (CardRarity(Common, Uncommon, Rare, XRare, URare, Promo, Starter))
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

type alias Filters =
    { rarity : List CardRarity
    }

type alias Model =
    { location : Maybe Route
    , cards : CardList
    , deck : Deck
    , card : Maybe String
    , filters : Filters
    }


type Msg
    = NavigateTo String
    | SetRoute (Maybe Route)
    | CardsLoaded (Result Http.Error CardList)
    | Decrement String
    | Increment String
    | ExportDeck
    -- TODO: differentiate between filter types
    | AddFilter CardRarity
    | RemoveFilter CardRarity


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


typeOrder : CardType -> Int
typeOrder cardType =
    case cardType of
        Character ->
            1

        Battle (Strength _) ->
            2

        Battle (Intelligence _) ->
            3

        Battle (Special _) ->
            4

        Battle (Multi _) ->
            5

        Event ->
            6

        Unknown ->
            7


cardListSort : Comparator Card
cardListSort =
    concat
        [ by (typeOrder << .card_type)
        , by .title
        , by (effectToString << .effect)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute route ->
            case route of
                Just Route.Home ->
                    ( { model | location = route }, Cmd.none )

                Just Route.Deck ->
                    ( { model | location = route }, Cmd.none )

                Just (Route.Card cardId) ->
                    ( { model | location = route, card = Just cardId }, Cmd.none )

                Just Route.Search ->
                    ( { model | location = route }, Cmd.none )

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

        ExportDeck ->
            ( model, Request.Deck.export model.cards model.deck )

        AddFilter rarity ->
            let
                updatedRarity = rarity :: model.filters.rarity
            in
                ( { model | filters = { rarity = updatedRarity } }, Cmd.none )

        RemoveFilter rarity ->
            let
                updatedRarity = List.filter ((/=) rarity) model.filters.rarity
            in
                ( { model | filters = { rarity = updatedRarity } }, Cmd.none )


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
        [ img [ src "/icons/logo.png" ] []
        , text title
        ]


getNavbarIcon : Maybe Route -> Html Msg
getNavbarIcon location =
    case location of
        Just Route.Home ->
            button [ class "navbar-button"
                   , href ("/search")
                   , onNavigate (NavigateTo "/search") ]
                [ img [ src "/icons/ios-search-white.svg" ] [] ]

        Just Route.Deck ->
            button [ class "navbar-button", onClick ExportDeck ]
                [ img [ src "/icons/ios-download-outline.svg" ] [] ]

        Just (Route.Card _) ->
            text ""

        Just Route.Search ->
            text ""

        Nothing ->
            text ""

navbarTop : Model -> Html Msg
navbarTop model =
    nav [ class "navbar-top" ]
        [ logo "MetaX DB"
        , getNavbarIcon model.location
        ]


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

-- TODO: These should go somewhere else
filterRarity : CardRarity -> Card -> Bool
filterRarity rarity card =
    card.rarity == rarity


applyFilters : Filters -> Card -> Bool
applyFilters filters card =
    List.any (\rarity -> filterRarity rarity card) filters.rarity


cardListPane : Model -> Html Msg
cardListPane model =
    div
        [ id "card-list-pane"
        , class "pane"
        ]
        (List.map (cardView model) (List.filter (applyFilters model.filters) model.cards))


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


cardEffect : CardEffect -> Html Msg
cardEffect effect =
    div [ class "card-effect" ] (effectToHtml effect)


cardText : Card -> Html Msg
cardText card =
    div [ class "card-text" ]
        [ div [ class "card-title" ] [ text card.title ]
        , cardEffect card.effect
        ]


cardStats : Card -> Html Msg
cardStats card =
    div [ class "card-stats" ]
        [ mpView card.mp
        , statView "strength" card.strength
        , statView "intelligence" card.intelligence
        , statView "special" card.special
        ]


cardDetails : Card -> Html Msg
cardDetails card =
    div [ class "card-details" ]
        [ a
            [ class "card-thumbnail"
            , href ("/card/" ++ card.id)
            , onNavigate (NavigateTo ("/card/" ++ card.id))
            ]
            [ img [ src (replace Regex.All (regex "/images/") (\_ -> "/thumbnails/") card.image_url) ] []
            ]
        , cardText card
        , cardStats card
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


type alias BattleCardGroups =
    { strength : Dict Int (List ( Maybe Card, Int ))
    , intelligence : Dict Int (List ( Maybe Card, Int ))
    , special : Dict Int (List ( Maybe Card, Int ))
    , multi : Dict Int (List ( Maybe Card, Int ))
    }


addToRank : ( Maybe Card, Int ) -> Maybe (List ( Maybe Card, Int )) -> Maybe (List ( Maybe Card, Int ))
addToRank item list =
    case list of
        Just list ->
            Just (item :: list)

        Nothing ->
            Just [ item ]


groupBattleCards : ( Maybe Card, Int ) -> BattleCardGroups -> BattleCardGroups
groupBattleCards ( card, count ) result =
    case card of
        Just { card_type } ->
            case card_type of
                Battle battleType ->
                    case battleType of
                        Strength rank ->
                            { result | strength = Dict.update rank (addToRank ( card, count )) result.strength }

                        Intelligence rank ->
                            { result | intelligence = Dict.update rank (addToRank ( card, count )) result.intelligence }

                        Special rank ->
                            { result | special = Dict.update rank (addToRank ( card, count )) result.special }

                        Multi rank ->
                            { result | multi = Dict.update rank (addToRank ( card, count )) result.multi }

                Character ->
                    result

                Event ->
                    result

                Unknown ->
                    result

        Nothing ->
            result


toRows : String -> Int -> List ( Maybe Card, Int ) -> List (Html Msg) -> List (Html Msg)
toRows title rank cards result =
    List.append result (battleCardSubSection (title ++ "- Rank " ++ (toString rank)) cards)


battleCardView : List ( Maybe Card, Int ) -> List (Html Msg)
battleCardView battle =
    if List.length battle > 0 then
        let
            rows =
                List.foldl groupBattleCards (BattleCardGroups Dict.empty Dict.empty Dict.empty Dict.empty) battle

            strRows =
                Dict.foldl (toRows "Strength") [] rows.strength

            intRows =
                Dict.foldl (toRows "Intelligence") [] rows.intelligence

            spRows =
                Dict.foldl (toRows "Special") [] rows.special

            multiRows =
                Dict.foldl (toRows "Multi") [] rows.multi
        in
            (sectionHeader "Battle Cards" (sum battle))
                ++ strRows
                ++ intRows
                ++ spRows
                ++ multiRows
    else
        []


type alias DeckGroups =
    { characters : List ( Maybe Card, Int )
    , events : List ( Maybe Card, Int )
    , battle : List ( Maybe Card, Int )
    }


groupTypes : ( Maybe Card, Int ) -> DeckGroups -> DeckGroups
groupTypes ( card, count ) result =
    case card of
        Just { card_type } ->
            case card_type of
                Character ->
                    { result | characters = ( card, count ) :: result.characters }

                Event ->
                    { result | events = ( card, count ) :: result.events }

                Battle _ ->
                    { result | battle = ( card, count ) :: result.battle }

                Unknown ->
                    result

        Nothing ->
            result


deckSectionView : List ( Maybe Card, Int ) -> List (Html Msg)
deckSectionView cards =
    let
        rows =
            List.foldl groupTypes (DeckGroups [] [] []) cards
    in
        List.concat
            [ charactersView rows.characters
            , eventsView rows.events
            , battleCardView rows.battle
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
                    [ a
                        [ class "card-title"
                        , href ("/card/" ++ card.id)
                        , onNavigate (NavigateTo ("/card/" ++ card.id))
                        ]
                        [ img [ src "/icons/ios-search.svg", class "view-icon" ] []
                        , text ("(" ++ card.id ++ ") ")
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


cardPane : Maybe String -> Model -> Html Msg
cardPane cardId model =
    case cardId of
        Just cardId ->
            let
                card =
                    lookup model cardId

                count =
                    Maybe.withDefault 0 (Dict.get cardId model.deck)
            in
                case card of
                    Just card ->
                        div
                            [ id "card-pane"
                            , class "pane"
                            ]
                            [ img
                                [ class "card-full"
                                , src card.image_url
                                ]
                                []
                            , div [ class "card-details" ]
                                [ cardText card
                                , cardStats card
                                ]
                            , stepper ( card, count )
                            ]

                    Nothing ->
                        div
                            [ id "card-pane"
                            , class "pane align-center"
                            ]
                            [ text "Card not found"
                            ]

        Nothing ->
            div
                [ id "card-pane"
                , class "pane align-center"
                ]
                [ text "Click a card image to view" ]


getClassList : Maybe Route -> List ( String, Bool )
getClassList location =
    case location of
        Just Route.Home ->
            [ ( "pane-container", True ) ]

        Just Route.Deck ->
            [ ( "pane-container", True ), ( "is-deck", True ) ]

        Just (Route.Card _) ->
            [ ( "pane-container", True ), ( "is-card", True ) ]

        Just Route.Search ->
            [ ( "pane-container", True ), ( "is-search", True ) ]

        Nothing ->
            [ ( "pane-container", True ) ]


getCardId : Maybe Route -> Maybe String
getCardId location =
    case location of
        Just Route.Home ->
            Nothing

        Just Route.Deck ->
            Nothing

        Just (Route.Card id) ->
            Just id

        Just Route.Search ->
            Nothing

        Nothing ->
            Nothing


updateFilter : CardRarity -> Bool -> Msg
updateFilter rarity isChecked =
    if isChecked then AddFilter rarity
    else RemoveFilter rarity


checkbox : String -> CardRarity -> List CardRarity -> Html Msg
checkbox label_ rarity rarityFilters =
    let
        isChecked = List.member rarity rarityFilters
    in
        label [ class "checkbox" ]
            [ input [ type_ "checkbox", checked isChecked, onCheck (updateFilter rarity) ] []
            , text label_
            ]


searchPane : Model -> Html Msg
searchPane model =
    div [ id "search-pane"
        , class "pane"
        ]
        [ div [ class "list-item-header"] [ text "Rarity" ]
        , checkbox "Common" Common model.filters.rarity
        , checkbox "Uncommon" Uncommon model.filters.rarity
        , checkbox "Rare" Rare model.filters.rarity
        , checkbox "XR" XRare model.filters.rarity
        , checkbox "UR" URare model.filters.rarity
        , checkbox "Promo" Promo model.filters.rarity
        , checkbox "Starter" Starter model.filters.rarity
        , button [ class "search-button"
                 , href ("/")
                 , onNavigate (NavigateTo "/")] [ text "Search" ]
        , button [ class "auto-button", disabled True ] [ text "Auto Filtering" ]
        ]


paneContainer : Model -> Html Msg
paneContainer model =
    let
        containerClasses =
            getClassList model.location

        cardId =
            getCardId model.location
    in
        div [ classList containerClasses ]
            [ cardListPane model
            , deckListPane model
            , searchPane model
            , cardPane cardId model
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
      , filters =
          { rarity = [Common, Uncommon, Rare, XRare, URare]
          }
      }
    , Request.CardList.load
        |> Http.send CardsLoaded
    )
