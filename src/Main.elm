module Main exposing (Model, Msg, update, view, subscriptions, init)

import Http
import Html exposing (nav, div, img, text, button, a, span, label, input, Html)
import Html.Attributes exposing (href, class, classList, id, src, disabled, type_, placeholder, value, checked)
import Html.Events exposing (onClick, onCheck)
import Navigation exposing (newUrl, Location)
import Dict exposing (Dict)
import Regex exposing (regex, contains, replace, Regex)
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
import Ports exposing (onSessionLoaded, loadSession)


main : Program Never Model Msg
main =
    Navigation.program (fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { locationTo : Maybe Route
    , locationFrom : Maybe Route
    , cards : CardList
    , deck : Deck
    , card : Maybe String
    , filterRarity : List CardRarity
    , filterType : List CardType
    , rarityOpen : Bool
    }


type Msg
    = NavigateTo String
    | SetRoute (Maybe Route)
    | CardsLoaded (Result Http.Error CardList)
    | Decrement String
    | Increment String
    | ExportDeck
      -- TODO: differentiate between filter types
    | AddRarityFilter CardRarity
    | RemoveRarityFilter CardRarity
    | AddTypeFilter CardType
    | RemoveTypeFilter CardType
    | LoadDeck Deck
    | ToggleOpenRarity


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
                    ( { model | locationFrom = model.locationTo, locationTo = route }, Cmd.none )

                Just Route.Deck ->
                    ( { model | locationFrom = model.locationTo, locationTo = route }, Cmd.none )

                Just (Route.Card cardId) ->
                    ( { model | locationFrom = model.locationTo, locationTo = route, card = Just cardId }, Cmd.none )

                Just Route.Search ->
                    ( { model | locationFrom = model.locationTo, locationTo = route }, Cmd.none )

                Nothing ->
                    ( { model | locationFrom = model.locationTo, locationTo = Just Route.Home }, Cmd.none )

        NavigateTo pathname ->
            ( model, newUrl pathname )

        LoadDeck deck ->
            ( { model | deck = deck }, Cmd.none )

        CardsLoaded (Ok cards) ->
            -- TODO: Deck ID
            ( { model | cards = (List.sortWith cardListSort cards) }, loadSession "" )

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

        AddRarityFilter rarity ->
            let
                updatedRarity =
                    rarity :: model.filterRarity
            in
                ( { model | filterRarity = updatedRarity }, Cmd.none )

        RemoveRarityFilter rarity ->
            let
                updatedRarity =
                    List.filter ((/=) rarity) model.filterRarity
            in
                ( { model | filterRarity = updatedRarity }, Cmd.none )

        AddTypeFilter cardType ->
            let
                updatedType =
                    cardType :: model.filterType
            in
                ( { model | filterType = updatedType }, Cmd.none )

        RemoveTypeFilter cardType ->
            let
                updatedType =
                    List.filter ((/=) cardType) model.filterType
            in
                ( { model | filterType = updatedType }, Cmd.none )

        ToggleOpenRarity ->
            ( { model | rarityOpen = not model.rarityOpen }, Cmd.none )


view : Model -> Html Msg
view model =
    applicationShell model


linkTo : String -> (List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg)
linkTo pathname =
    let
        linkAttrs =
            [ href pathname
            , onNavigate (NavigateTo pathname)
            ]
    in
        (\attrs contents -> a (List.append attrs linkAttrs) contents)


logo : String -> Html Msg
logo title =
    div [ class "navitem logo" ]
        [ img [ src "/icons/logo.png" ] []
        , text title
        ]


downloadButton : Html Msg
downloadButton =
    button [ class "navbar-button", onClick ExportDeck ]
           [ img [ src "/icons/ios-download-outline.svg" ] [] ]

getNavbarIcon : Maybe Route -> Html Msg
getNavbarIcon location =
    case location of
        Just Route.Home ->
            linkTo "/search"
                [ class "navbar-button" ]
                [ img [ src "/icons/ios-search-white.svg" ] [] ]

        Just Route.Deck ->
            downloadButton

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
        , getNavbarIcon <| .locationTo model
        ]


decklistText : Model -> Html Msg
decklistText model =
    let
        deckContents =
            (List.map (Tuple.mapFirst (lookup model)) (Dict.toList model.deck))

        deckSize =
            sum deckContents
    in
        text ("Deck (" ++ (toString deckSize) ++ ")")


navbarBottom : Model -> Html Msg
navbarBottom model =
    nav [ class "navbar-bottom" ]
        [ linkTo "/" [ class "navitem" ] [ text "Cards" ]
        , linkTo "/deck" [ class "navitem" ] [ decklistText model ]
        ]



-- TODO: These should go somewhere else


filterRarity : CardRarity -> Card -> Bool
filterRarity rarity card =
    card.rarity == rarity

filterType : CardType -> Card -> Bool
filterType cardType card =
    card.card_type == cardType

type alias Filters filters =
    { filters | filterRarity : List CardRarity, filterType : List CardType }

applyRarityFilters : List CardRarity -> Card -> Bool
applyRarityFilters filters card =
    List.any (\rarity -> filterRarity rarity card) filters

applyTypeFilters : List CardType -> Card -> Bool
applyTypeFilters filters card =
    List.any (\cardType -> filterType cardType card) filters

applyFilters : Filters filters -> Card -> Bool
applyFilters filters card =
     (applyRarityFilters filters.filterRarity card) && (applyTypeFilters filters.filterType card)


cardListPane : Model -> Html Msg
cardListPane model =
    div
        [ id "card-list-pane"
        , class "pane"
        ]
        (List.map (cardView model) (List.filter (applyFilters model) model.cards))


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
        [ linkTo ("/card/" ++ card.id)
            [ class "card-thumbnail" ]
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


deckSectionView : List ( Maybe Card, Int ) -> Html Msg
deckSectionView cards =
    let
        rows =
            List.foldl groupTypes (DeckGroups [] [] []) cards
    in
        div [ class "column-contents" ]
            (List.concat
                [ charactersView rows.characters
                , eventsView rows.events
                , battleCardView rows.battle
                ])


deckCardView : ( Maybe Card, Int ) -> Html Msg
deckCardView card =
    case card of
        ( Just card, count ) ->
            div
                [ id ("deck_" ++ card.id)
                , class "list-item"
                ]
                [ div [ class "deck-card-details" ]
                    [ linkTo ("/card/" ++ card.id)
                        [ class "card-title" ]
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
        , class "pane column-wrapper"
        ]
        -- TODO: use |> operator
        [ div [ class "column-footer" ]
              [ div [ class "column-label" ] [ decklistText model ]
              , downloadButton
              ]
        , deckSectionView (List.map (Tuple.mapFirst (lookup model)) (Dict.toList model.deck))
        ]

cardPane : Model -> Html Msg
cardPane model =
    case .card model of
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


getClassList : ( Maybe Route, Maybe Route ) -> List ( String, Bool )
getClassList ( from, to ) =
    case ( from, to ) of
        -- From Nothing
        ( Nothing, Just Route.Home ) ->
            [ ( "pane-container", True ), ( "to-home", True ) ]

        ( Nothing, Just Route.Deck ) ->
            [ ( "pane-container", True ), ( "to-deck", True ) ]

        ( Nothing, Just (Route.Card _) ) ->
            [ ( "pane-container", True ), ( "to-card", True ) ]

        ( Nothing, Just Route.Search ) ->
            [ ( "pane-container", True ), ( "to-search", True ) ]

        -- From Home
        ( Just Route.Home, Just Route.Home ) ->
            [ ( "pane-container", True ), ( "from-home", True ), ( "to-home", True ) ]

        ( Just Route.Home, Just Route.Deck ) ->
            [ ( "pane-container", True ), ( "from-home", True ), ( "to-deck", True ) ]

        ( Just Route.Home, Just (Route.Card _) ) ->
            [ ( "pane-container", True ), ( "from-home", True ), ( "to-card", True ) ]

        ( Just Route.Home, Just Route.Search ) ->
            [ ( "pane-container", True ), ( "from-home", True ), ( "to-search", True ) ]

        -- From Deck
        ( Just Route.Deck, Just Route.Home ) ->
            [ ( "pane-container", True ), ( "from-deck", True ), ( "to-home", True ) ]

        ( Just Route.Deck, Just Route.Deck ) ->
            [ ( "pane-container", True ), ( "from-deck", True ), ( "to-deck", True ) ]

        ( Just Route.Deck, Just (Route.Card _) ) ->
            [ ( "pane-container", True ), ( "from-deck", True ), ( "to-card", True ) ]

        ( Just Route.Deck, Just Route.Search ) ->
            [ ( "pane-container", True ), ( "from-deck", True ), ( "to-search", True ) ]

        -- From Card
        ( Just (Route.Card _), Just Route.Home ) ->
            [ ( "pane-container", True ), ( "from-card", True ), ( "to-home", True ) ]

        ( Just (Route.Card _), Just Route.Deck ) ->
            [ ( "pane-container", True ), ( "from-card", True ), ( "to-deck", True ) ]

        ( Just (Route.Card _), Just (Route.Card _) ) ->
            [ ( "pane-container", True ), ( "from-card", True ), ( "to-card", True ) ]

        ( Just (Route.Card _), Just Route.Search ) ->
            [ ( "pane-container", True ), ( "from-card", True ), ( "to-search", True ) ]

        -- From Search
        ( Just Route.Search, Just Route.Home ) ->
            [ ( "pane-container", True ), ( "from-search", True ), ( "to-home", True ) ]

        ( Just Route.Search, Just Route.Deck ) ->
            [ ( "pane-container", True ), ( "from-search", True ), ( "to-deck", True ) ]

        ( Just Route.Search, Just (Route.Card _) ) ->
            [ ( "pane-container", True ), ( "from-search", True ), ( "to-card", True ) ]

        ( Just Route.Search, Just Route.Search ) ->
            [ ( "pane-container", True ), ( "from-search", True ), ( "to-search", True ) ]

        -- To Nothing
        ( _, Nothing ) ->
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


updateRarityFilter : CardRarity -> Bool -> Msg
updateRarityFilter rarity isChecked =
    if isChecked then
        AddRarityFilter rarity
    else
        RemoveRarityFilter rarity

updateTypeFilter : CardType -> Bool -> Msg
updateTypeFilter cardType isChecked =
    if isChecked then
        AddTypeFilter cardType
    else
        RemoveTypeFilter cardType


checkbox : String -> Bool -> (Bool -> Msg) -> Html Msg
checkbox label_ isChecked msg =
    label [ class "checkbox" ]
        [ input [ type_ "checkbox", checked isChecked, onCheck msg ] []
        , text label_
        ]

rarityToString : CardRarity -> String
rarityToString rarity =
    case rarity of
        Common ->
            "C"
        Uncommon ->
            "UC"
        Rare ->
            "R"
        XRare ->
            "XR"
        URare ->
            "UR"
        Starter ->
            "S"
        Promo ->
            "P"
        -- TODO: Blah! Remove "Unknown"
        _ ->
            ""

buildQuery : Model -> String
buildQuery model =
    if List.length model.filterRarity == 0 then ""
    else (++) "rarity:" (String.join "," <| List.map rarityToString model.filterRarity)

searchPane : Model -> Html Msg
searchPane model =
    div
        [ id "search-pane"
        , class "pane"
        ]
        [ input [ type_ "search", placeholder "Search", class "search-box", value (buildQuery model) ] []
        , div [ class "help-text" ] [ text "Need help?" ]
        -- TODO: maybe a button?
        -- , div [ class "signpost" ] [ text "Card Type" ]
        -- , div [ class "signpost" ] [ text "Rank" ]
        , div [ classList [("option-container", True), ("is-open", model.rarityOpen)] ]
            [ div [ class "option-title", onClick ToggleOpenRarity ] [ text "Rarity" ]
            , div [ class "option-body" ]
                [ checkbox "Common" (List.member Common model.filterRarity) (updateRarityFilter Common)
                , checkbox "Uncommon" (List.member Uncommon model.filterRarity) (updateRarityFilter Uncommon)
                , checkbox "Rare" (List.member Rare model.filterRarity) (updateRarityFilter Rare)
                , checkbox "XR" (List.member XRare model.filterRarity) (updateRarityFilter XRare)
                , checkbox "UR" (List.member URare model.filterRarity) (updateRarityFilter URare)
                , checkbox "Promo" (List.member Promo model.filterRarity) (updateRarityFilter Promo)
                , checkbox "Starter" (List.member Starter model.filterRarity) (updateRarityFilter Starter)
                ]
            ]
        --     div [ class "list-item-header" ] [ text "Rarity" ]
        -- , div [ class "list-item-header" ] [ text "Rarity" ]
        -- , checkbox "Character" (List.member Character model.filterType) (updateTypeFilter Character)
        -- , checkbox "Event" (List.member Event model.filterType) (updateTypeFilter Event)
        -- , checkbox "Battle - Strength 1" (List.member (Battle <| Strength 1) model.filterType) (updateTypeFilter (Battle <| Strength 1))
        -- , checkbox "Battle - Strength 2" (List.member (Battle <| Strength 2) model.filterType) (updateTypeFilter (Battle <| Strength 2))
        -- , checkbox "Battle - Strength 3" (List.member (Battle <| Strength 3) model.filterType) (updateTypeFilter (Battle <| Strength 3))
        -- , checkbox "Battle - Strength 4" (List.member (Battle <| Strength 4) model.filterType) (updateTypeFilter (Battle <| Strength 4))
        -- , checkbox "Battle - Strength 5" (List.member (Battle <| Strength 5) model.filterType) (updateTypeFilter (Battle <| Strength 5))
        -- , checkbox "Battle - Strength 6" (List.member (Battle <| Strength 6) model.filterType) (updateTypeFilter (Battle <| Strength 6))
        -- , checkbox "Battle - Strength 7" (List.member (Battle <| Strength 7) model.filterType) (updateTypeFilter (Battle <| Strength 7))
        -- , checkbox "Battle - Int 1" (List.member (Battle <| Intelligence 1) model.filterType) (updateTypeFilter (Battle <| Intelligence 1))
        -- , checkbox "Battle - Int 2" (List.member (Battle <| Intelligence 2) model.filterType) (updateTypeFilter (Battle <| Intelligence 2))
        -- , checkbox "Battle - Int 3" (List.member (Battle <| Intelligence 3) model.filterType) (updateTypeFilter (Battle <| Intelligence 3))
        -- , checkbox "Battle - Int 4" (List.member (Battle <| Intelligence 4) model.filterType) (updateTypeFilter (Battle <| Intelligence 4))
        -- , checkbox "Battle - Int 5" (List.member (Battle <| Intelligence 5) model.filterType) (updateTypeFilter (Battle <| Intelligence 5))
        -- , checkbox "Battle - Int 6" (List.member (Battle <| Intelligence 6) model.filterType) (updateTypeFilter (Battle <| Intelligence 6))
        -- , checkbox "Battle - Int 7" (List.member (Battle <| Intelligence 7) model.filterType) (updateTypeFilter (Battle <| Intelligence 7))
        -- , checkbox "Battle - Special 1" (List.member (Battle <| Special 1) model.filterType) (updateTypeFilter (Battle <| Special 1))
        -- , checkbox "Battle - Special 2" (List.member (Battle <| Special 2) model.filterType) (updateTypeFilter (Battle <| Special 2))
        -- , checkbox "Battle - Special 3" (List.member (Battle <| Special 3) model.filterType) (updateTypeFilter (Battle <| Special 3))
        -- , checkbox "Battle - Special 4" (List.member (Battle <| Special 4) model.filterType) (updateTypeFilter (Battle <| Special 4))
        -- , checkbox "Battle - Special 5" (List.member (Battle <| Special 5) model.filterType) (updateTypeFilter (Battle <| Special 5))
        -- , checkbox "Battle - Special 6" (List.member (Battle <| Special 6) model.filterType) (updateTypeFilter (Battle <| Special 6))
        -- , checkbox "Battle - Special 7" (List.member (Battle <| Special 7) model.filterType) (updateTypeFilter (Battle <| Special 7))
        -- , checkbox "Battle - Multi 1" (List.member (Battle <| Multi 1) model.filterType) (updateTypeFilter (Battle <| Multi 1))
        -- , checkbox "Battle - Multi 2" (List.member (Battle <| Multi 2) model.filterType) (updateTypeFilter (Battle <| Multi 2))
        -- , checkbox "Battle - Multi 3" (List.member (Battle <| Multi 3) model.filterType) (updateTypeFilter (Battle <| Multi 3))
        -- , checkbox "Battle - Multi 4" (List.member (Battle <| Multi 4) model.filterType) (updateTypeFilter (Battle <| Multi 4))
        -- , checkbox "Battle - Multi 5" (List.member (Battle <| Multi 5) model.filterType) (updateTypeFilter (Battle <| Multi 5))
        -- , checkbox "Battle - Multi 6" (List.member (Battle <| Multi 6) model.filterType) (updateTypeFilter (Battle <| Multi 6))
        -- , checkbox "Battle - Multi 7" (List.member (Battle <| Multi 7) model.filterType) (updateTypeFilter (Battle <| Multi 7))
        , button
            [ class "search-button"
            , href ("/")
            , onNavigate (NavigateTo "/")
            ]
            [ text "Search" ]
        -- , button [ class "auto-button", disabled True ] [ text "Auto Filtering" ]
        ]


paneContainer : Model -> Html Msg
paneContainer model =
    let
        containerClasses =
            getClassList <| ( .locationFrom model, .locationTo model )
    in
        div [ classList containerClasses ]
            [ cardListPane model
            , deckListPane model
            , searchPane model
            , cardPane model
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
    Sub.map LoadDeck (onSessionLoaded Deck.decoder)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            fromLocation location
    in
        ( { locationTo = route
          , locationFrom = Nothing
          , cards = []
          , card = getCardId route
          , deck = Dict.empty
          , filterRarity = [ Common, Uncommon, Rare, XRare, URare ]
          , filterType =
            [ -- TODO: This isn't flexible
              Character
            , Event
            , Battle (Strength 1)
            , Battle (Strength 2)
            , Battle (Strength 3)
            , Battle (Strength 4)
            , Battle (Strength 5)
            , Battle (Strength 6)
            , Battle (Strength 7)
            , Battle (Intelligence 1)
            , Battle (Intelligence 2)
            , Battle (Intelligence 3)
            , Battle (Intelligence 4)
            , Battle (Intelligence 5)
            , Battle (Intelligence 6)
            , Battle (Intelligence 7)
            , Battle (Special 1)
            , Battle (Special 2)
            , Battle (Special 3)
            , Battle (Special 4)
            , Battle (Special 5)
            , Battle (Special 6)
            , Battle (Special 7)
            , Battle (Multi 1)
            , Battle (Multi 2)
            , Battle (Multi 3)
            , Battle (Multi 4)
            , Battle (Multi 5)
            , Battle (Multi 6)
            , Battle (Multi 7)
            ]
          , rarityOpen = False
          }
        , Request.CardList.load
            |> Http.send CardsLoaded
        )
