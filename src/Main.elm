module Main exposing (Model, Msg, update, view, subscriptions, init)

import Http
import Html exposing (header, nav, div, img, text, button, a, span, label, input, li, ul, Html)
import Html.Attributes exposing (href, class, classList, id, src, alt, disabled, type_, placeholder, value, checked)
import Html.Events exposing (onClick, onCheck)
import Navigation exposing (newUrl, modifyUrl, Location)
import Dict exposing (Dict)
import Regex exposing (regex, contains, replace, Regex)
import Data.Card as Card exposing (Card)
import Data.CardList as CardList exposing (CardList)
import Data.CardType exposing (CardType(..))
import Data.CardEffect exposing (CardEffect(..), effectToString, effectToHtml)
import Data.CardRarity exposing (CardRarity(Common, Uncommon, Rare, XRare, URare, Promo, Starter), cardRarityToString)
import Data.CardSet exposing (CardSet(JL, GL, AT), cardSetToString)
import Data.CardStatList exposing (CardStatList)
import Data.CardStat as CardStat exposing (CardStat(Strength, Intelligence, Special))
import Data.Deck as Deck exposing (Deck)
import Request.Deck
import Request.CardList
import Util exposing (onNavigate)
import Compare exposing (concat, by, Comparator)
import Route exposing (fromLocation, Route)
import Ports exposing (onSessionLoaded, loadSession)
import GraphQl as GQL
import Encode
import RouteUrl exposing (RouteUrlProgram, UrlChange, HistoryEntry(NewEntry, ModifyEntry))


main : RouteUrlProgram Never Model Msg
main =
    RouteUrl.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Attribution =
    { name : String
    , author : String
    , link : String
    }


type alias Model =
    { locationTo : Maybe Route
    , locationFrom : Maybe Route
    , hash : String
    , cards : CardList
    , deck : Deck
    , card : Maybe String
    , filterRarity : List CardRarity
    , filterSet : List CardSet
    , filterType : List CardType
    , rarityOpen : Bool
    , setOpen : Bool
    , patrons : List String
    , attributions : List Attribution
    }


type Msg
    = SetRoute Route
    | CardsLoaded (Result Http.Error CardList)
    | Decrement String
    | Increment String
    | ExportDeck
    | AddRarityFilter CardRarity
    | RemoveRarityFilter CardRarity
    | AddSetFilter CardSet
    | RemoveSetFilter CardSet
      -- Currently unused
    | AddTypeFilter CardType
    | RemoveTypeFilter CardType
    | LoadDeck Deck
    | ToggleOpenRarity
    | ToggleOpenSet


delta2url : Model -> Model -> Maybe UrlChange
delta2url prevModel nextModel =
    let
        hash =
            hashDeck nextModel nextModel.deck

        querystring =
            hashQuery hash
    in
        case nextModel.locationTo of
            Just Route.Home ->
                Just
                    { entry = NewEntry
                    , url = "/" ++ querystring
                    }

            Just Route.Deck ->
                Just
                    { entry = NewEntry
                    , url = "/deck" ++ querystring
                    }

            Just (Route.Card uid) ->
                Just
                    { entry = NewEntry
                    , url = "/card/" ++ uid ++ querystring
                    }

            Just Route.Search ->
                Just
                    { entry = NewEntry
                    , url = "/search" ++ querystring
                    }

            Just Route.Info ->
                Just
                    { entry = NewEntry
                    , url = "/info" ++ querystring
                    }

            Nothing ->
                Just
                    { entry = ModifyEntry
                    , url = querystring
                    }


location2messages : Location -> List Msg
location2messages location =
    let
        route =
            fromLocation location
    in
        case route of
            Just route ->
                [ SetRoute route ]

            Nothing ->
                []


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


battleTypeOrder : Card -> Int
battleTypeOrder { card_type, stats } =
    -- That hackiness, tho
    if card_type /= Battle then
        0
    else
        case toBattleType stats of
            Just (Strength rank) ->
                0 + rank

            Just (Intelligence rank) ->
                7 + rank

            Just (Special rank) ->
                14 + rank

            Just (Multi rank) ->
                21 + rank

            Nothing ->
                28


typeOrder : CardType -> Int
typeOrder cardType =
    case cardType of
        Character ->
            1

        Battle ->
            2

        Event ->
            3


cardListSort : Comparator Card
cardListSort =
    concat
        [ by (typeOrder << .card_type)
        , by battleTypeOrder
        , by .title
        , by (effectToString << .effect)
        ]


hashDeck : Model -> Deck -> Maybe String
hashDeck model deck =
    Encode.hash (List.map (Tuple.mapFirst (lookup model)) (Dict.toList deck))


hashQuery : Maybe String -> String
hashQuery maybeHash =
    Maybe.withDefault "?" <| Maybe.map (\h -> "?deck=" ++ h) maybeHash


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute route ->
            case route of
                Route.Home ->
                    ( { model | locationFrom = model.locationTo, locationTo = Just route }, Cmd.none )

                Route.Deck ->
                    ( { model | locationFrom = model.locationTo, locationTo = Just route }, Cmd.none )

                Route.Card cardId ->
                    ( { model | locationFrom = model.locationTo, locationTo = Just route, card = Just cardId }, Cmd.none )

                Route.Search ->
                    ( { model | locationFrom = model.locationTo, locationTo = Just route }, Cmd.none )

                Route.Info ->
                    ( { model | locationFrom = model.locationTo, locationTo = Just route }, Cmd.none )

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

        AddSetFilter set ->
            let
                updatedSet =
                    set :: model.filterSet
            in
                ( { model | filterSet = updatedSet }, Cmd.none )

        RemoveSetFilter set ->
            let
                updatedSet =
                    List.filter ((/=) set) model.filterSet
            in
                ( { model | filterSet = updatedSet }, Cmd.none )

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

        ToggleOpenSet ->
            ( { model | setOpen = not model.setOpen }, Cmd.none )


view : Model -> Html Msg
view model =
    applicationShell model


linkTo : Route -> (List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg)
linkTo pathname =
    let
        linkAttrs =
            -- TODO: fix href
            [ href ""
            , onNavigate (SetRoute pathname)
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
    button [ class "navbar-button export-button", onClick ExportDeck ]
        [ img [ src "/icons/ios-download-outline.svg" ] [] ]


getNavbarIcon : Maybe Route -> Html Msg
getNavbarIcon location =
    case location of
        Just Route.Home ->
            text ""

        Just Route.Deck ->
            downloadButton

        Just (Route.Card _) ->
            text ""

        Just Route.Search ->
            text ""

        Just Route.Info ->
            text ""

        Nothing ->
            text ""


navbarTop : Model -> Html Msg
navbarTop model =
    header [ class "topbar" ]
        [ logo "MetaX DB"
        , getNavbarIcon <| .locationTo model
        ]


decklistText : Model -> Html Msg
decklistText model =
    text <| (deckSize model)


deckSize : Model -> String
deckSize model =
    let
        deckContents =
            (List.map (Tuple.mapFirst (lookup model)) (Dict.toList model.deck))

        size =
            sum deckContents
    in
        toString size


searchIcon : Model -> Html Msg
searchIcon model =
    let
        classes =
            case model.locationTo of
                Just Route.Search ->
                    "navitem navitem-search active"

                _ ->
                    "navitem navitem-search"
    in
        linkTo Route.Search [ class classes ] [ img [ class "navbar-icon", src "/icons/search.final.svg" ] [] ]


cardListIcon : Model -> Html Msg
cardListIcon model =
    let
        classes =
            case model.locationTo of
                Just Route.Home ->
                    "navitem navitem-cards active"

                _ ->
                    "navitem navitem-cards"
    in
        linkTo Route.Home [ class classes ] [ img [ class "navbar-icon", src "/icons/cards.final.svg" ] [] ]


deckIcon : Model -> Html Msg
deckIcon model =
    let
        classes =
            case model.locationTo of
                Just Route.Deck ->
                    "navitem navitem-deck active"

                _ ->
                    "navitem navitem-deck"
    in
        linkTo Route.Deck
            [ class classes ]
            [ img [ class "navbar-icon", src "/icons/deck.final.svg" ] []
            , span [ class "deck-size" ] [ decklistText model ]
            ]


infoIcon : Model -> Html Msg
infoIcon model =
    let
        classes =
            case model.locationTo of
                Just Route.Info ->
                    "navitem navitem-info active"

                _ ->
                    "navitem navitem-info"
    in
        linkTo Route.Info [ class classes ] [ img [ class "navbar-icon", src "/icons/info.final.svg" ] [] ]


navbarBottom : Model -> Html Msg
navbarBottom model =
    nav [ class "navbar" ]
        [ logo ""
        , searchIcon model
        , cardListIcon model
        , deckIcon model
        , infoIcon model
        , downloadButton
        ]



-- TODO: These should go somewhere else


filterRarity : CardRarity -> Card -> Bool
filterRarity rarity card =
    card.rarity == rarity


filterSet : CardSet -> Card -> Bool
filterSet set card =
    card.set == set


filterType : CardType -> Card -> Bool
filterType cardType card =
    card.card_type == cardType


type alias Filters filters =
    { filters | filterRarity : List CardRarity, filterSet : List CardSet, filterType : List CardType }


applyRarityFilters : List CardRarity -> Card -> Bool
applyRarityFilters filters card =
    List.any (\rarity -> filterRarity rarity card) filters


applySetFilters : List CardSet -> Card -> Bool
applySetFilters filters card =
    List.any (\set -> filterSet set card) filters


applyTypeFilters : List CardType -> Card -> Bool
applyTypeFilters filters card =
    List.any (\cardType -> filterType cardType card) filters


applyFilters : Filters filters -> Card -> Bool
applyFilters filters card =
    (applyRarityFilters filters.filterRarity card)
        && (applySetFilters filters.filterSet card)
        && (applyTypeFilters filters.filterType card)


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
            [ button [ class "stepper-button stepper-decrement ripple", disabled decrementDisabled, onClick (Decrement card.uid) ] [ text "-" ]
            , button [ class "stepper-button stepper-increment ripple", disabled incrementDisabled, onClick (Increment card.uid) ] [ text "+" ]
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


statsView : CardStatList -> List (Html Msg)
statsView stats =
    List.map statView stats


statView : CardStat -> Html Msg
statView stat =
    case stat of
        CardStat.Strength rank ->
            div [ class "card-stat" ]
                [ img [ class "card-stat-icon", src ("/icons/strength.png") ] []
                , span [ class "card-stat-text" ] [ text (toString rank) ]
                ]

        CardStat.Intelligence rank ->
            div [ class "card-stat" ]
                [ img [ class "card-stat-icon", src ("/icons/intelligence.png") ] []
                , span [ class "card-stat-text" ] [ text (toString rank) ]
                ]

        CardStat.Special rank ->
            div [ class "card-stat" ]
                [ img [ class "card-stat-icon", src ("/icons/special.png") ] []
                , span [ class "card-stat-text" ] [ text (toString rank) ]
                ]


cardEffect : CardEffect -> Html Msg
cardEffect effect =
    div [ class "card-effect" ] (effectToHtml effect)


cardTrait : String -> String
cardTrait trait =
    if trait == "" then
        ""
    else
        " (" ++ trait ++ ")"


cardText : Card -> Html Msg
cardText card =
    div [ class "card-text" ]
        [ div [ class "card-title" ]
            [ text card.title
            , text <| Maybe.withDefault "" <| Maybe.map (\s -> " - " ++ s) card.subtitle
            , text <| toBattleCardRank card
            , text <| cardTrait card.trait
            ]

        -- , cardTrait card.trait
        , cardEffect card.effect
        ]


cardStats : Card -> Html Msg
cardStats card =
    div [ class "card-stats" ]
        (mpView card.mp :: statsView card.stats)


previewBanner : Card -> Html Msg
previewBanner card =
    case card.preview of
        Just preview ->
            div [ class "preview-banner" ] [ text "Preview" ]

        Nothing ->
            text ""


cardDetails : Card -> Html Msg
cardDetails card =
    div [ class "card-details" ]
        [ linkTo (Route.Card card.uid)
            [ class "card-thumbnail" ]
            [ img [ src (replace Regex.All (regex "/images/") (\_ -> "/thumbnails/") card.image_url) ] []
            , previewBanner card
            ]
        , cardText card
        , cardStats card
        ]


cardView : Model -> Card -> Html Msg
cardView model card =
    let
        count =
            Maybe.withDefault 0 (Dict.get card.uid model.deck)
    in
        div
            [ id card.uid
            , class "list-item"
            ]
            [ cardDetails card
            , stepper ( card, count )
            ]


idMatches : String -> Card -> Bool
idMatches cardId card =
    card.uid == cardId


lookup : Model -> String -> Maybe Card
lookup model cardId =
    List.filter (idMatches cardId) model.cards
        |> List.head


sectionHeader : String -> Int -> List (Html Msg)
sectionHeader title count =
    List.singleton (div [ class "list-item-header" ] [ text <| title ++ " (" ++ (toString count) ++ ")" ])


sectionSubHeader : Html Msg -> List (Html Msg)
sectionSubHeader content =
    List.singleton (div [ class "list-item-sub-header" ] [ content ])


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


bcWarningView : Int -> Maybe (Html Msg)
bcWarningView count =
    if count > 3 then
        Just <| div [ class "warning-message" ] [ text "You have too many Battle Cards at this Type/Rank" ]
    else
        Nothing


battleCardSubSection : String -> List ( Maybe Card, Int ) -> List (Html Msg)
battleCardSubSection title cards =
    if List.length cards > 0 then
        let
            count =
                sum cards

            warning =
                bcWarningView count

            countString =
                toString count

            titleText =
                div [ class "subheader-title" ] [ text <| title ++ " (" ++ countString ++ ")" ]
        in
            case warning of
                Just warning ->
                    List.concat
                        [ List.singleton <| div [ class "list-item-sub-header with-warning" ] [ titleText, warning ]
                        , (List.map deckCardView cards)
                        ]

                Nothing ->
                    List.concat
                        [ List.singleton <| div [ class "list-item-sub-header" ] [ titleText ]
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


type BattleType
    = Strength Int
    | Intelligence Int
    | Special Int
    | Multi Int



-- | StrengthIntelligence Int
-- | StrengthSpecial Int
-- | IntelligenceSpecial Int
-- | StrengthIntelligenceSpecial Int


battleTypeFoldr : CardStat -> Maybe BattleType -> Maybe BattleType
battleTypeFoldr stat battleType =
    case ( battleType, stat ) of
        ( Nothing, CardStat.Strength rank ) ->
            Just (Strength rank)

        ( Nothing, CardStat.Intelligence rank ) ->
            Just (Intelligence rank)

        ( Nothing, CardStat.Special rank ) ->
            Just (Special rank)

        ( Just _, CardStat.Strength rank ) ->
            Just (Multi rank)

        ( Just _, CardStat.Intelligence rank ) ->
            Just (Multi rank)

        ( Just _, CardStat.Special rank ) ->
            Just (Multi rank)


toBattleType : CardStatList -> Maybe BattleType
toBattleType stats =
    List.foldr battleTypeFoldr Nothing stats


groupBattleCards : ( Maybe Card, Int ) -> BattleCardGroups -> BattleCardGroups
groupBattleCards ( card, count ) result =
    case card of
        Just { card_type, stats } ->
            case card_type of
                Battle ->
                    case toBattleType stats of
                        Just (Strength rank) ->
                            { result | strength = Dict.update rank (addToRank ( card, count )) result.strength }

                        Just (Intelligence rank) ->
                            { result | intelligence = Dict.update rank (addToRank ( card, count )) result.intelligence }

                        Just (Special rank) ->
                            { result | special = Dict.update rank (addToRank ( card, count )) result.special }

                        Just (Multi rank) ->
                            { result | multi = Dict.update rank (addToRank ( card, count )) result.multi }

                        Nothing ->
                            result

                Character ->
                    result

                Event ->
                    result

        Nothing ->
            result


toRows : String -> Int -> List ( Maybe Card, Int ) -> List (Html Msg) -> List (Html Msg)
toRows title rank cards result =
    List.append result (battleCardSubSection (title ++ " - Rank " ++ (toString rank)) cards)


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

                Battle ->
                    { result | battle = ( card, count ) :: result.battle }

        Nothing ->
            result


deckSectionView : List ( Maybe Card, Int ) -> List (Html Msg)
deckSectionView cards =
    let
        rows =
            List.foldl groupTypes (DeckGroups [] [] []) cards
    in
        (List.concat
            [ charactersView rows.characters
            , eventsView rows.events
            , battleCardView rows.battle
            ]
        )


toRank : CardStat -> Maybe Int -> Maybe Int
toRank stat rank =
    case stat of
        CardStat.Strength rank ->
            Just rank

        CardStat.Intelligence rank ->
            Just rank

        CardStat.Special rank ->
            Just rank


toBattleCardRank : Card -> String
toBattleCardRank card =
    let
        rank =
            if card.card_type == Battle then
                List.foldr toRank Nothing card.stats
            else
                Nothing
    in
        case rank of
            Just rank ->
                " - Rank " ++ (toString rank)

            Nothing ->
                ""


deckCardView : ( Maybe Card, Int ) -> Html Msg
deckCardView card =
    case card of
        ( Just card, count ) ->
            div
                [ id ("deck_" ++ card.uid)
                , class "list-item"
                ]
                [ cardDetails card
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
        (deckSectionView <| List.map (Tuple.mapFirst <| lookup model) <| Dict.toList model.deck)


previewedBy : Card -> Html Msg
previewedBy card =
    case card.preview of
        Just preview ->
            div [ class "preview-banner previewed-by" ] [ text "Previewed By: ", a [ href preview.previewUrl ] [ text preview.previewer ] ]

        Nothing ->
            text ""


largeImg : Card -> Html Msg
largeImg card =
    div [ class "card-full-wrapper" ]
        [ img [ class "card-full", src card.image_url ] []
        , previewedBy card
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
                            [ largeImg card
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
    let
        fromClass =
            case from of
                Just route ->
                    "from-" ++ Route.toClassString route

                Nothing ->
                    ""

        toClass =
            case to of
                Just route ->
                    "to-" ++ Route.toClassString route

                Nothing ->
                    ""
    in
        [ ( "pane-container", True ), ( fromClass, True ), ( toClass, True ) ]


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

        Just Route.Info ->
            Nothing

        Nothing ->
            Nothing


updateRarityFilter : CardRarity -> Bool -> Msg
updateRarityFilter rarity isChecked =
    if isChecked then
        AddRarityFilter rarity
    else
        RemoveRarityFilter rarity


updateSetFilter : CardSet -> Bool -> Msg
updateSetFilter set isChecked =
    if isChecked then
        AddSetFilter set
    else
        RemoveSetFilter set


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


buildQuery : Model -> String
buildQuery model =
    let
        rarityQuery =
            if List.length model.filterRarity == 0 then
                ""
            else
                (++) "rarity:" (String.join "," <| List.map cardRarityToString model.filterRarity)

        setQuery =
            if List.length model.filterSet == 0 then
                ""
            else
                (++) "set:" (String.join "," <| List.map cardSetToString model.filterSet)
    in
        -- TODO: this adds the space before if no rarities selected
        rarityQuery ++ " " ++ setQuery


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
        , div [ classList [ ( "option-container", True ), ( "is-open", model.rarityOpen ) ] ]
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
        , div [ classList [ ( "option-container", True ), ( "is-open", model.setOpen ) ] ]
            [ div [ class "option-title", onClick ToggleOpenSet ] [ text "Set" ]
            , div [ class "option-body" ]
                [ checkbox "Attack on Titan" (List.member AT model.filterSet) (updateSetFilter AT)
                , checkbox "Green Lantern" (List.member GL model.filterSet) (updateSetFilter GL)
                , checkbox "Justice Leauge" (List.member JL model.filterSet) (updateSetFilter JL)
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
            , onNavigate (SetRoute Route.Home)
            ]
            [ text "Search" ]
        ]


patreonView : Model -> Html Msg
patreonView model =
    div [ class "patreon" ]
        [ div [] [ text "Our work is supported by our Patrons on Patreon." ]
        , div []
            [ text "Extra "
            , img [ class "card-stat-icon", src "/icons/special.png" ] []
            , text " thanks our Rank 5 Patrons: "
            , span [ class "patreon-rank-5" ] (List.map text model.patrons)
            ]
        , div [ class "patreon" ] [ text "For development updates and extra features, subscribe to us on Patreon!" ]
        , a [ class "patreon-link", href "https://www.patreon.com/metaxdb" ] [ img [ src "/icons/patron.png", alt "Become a Patron" ] [] ]
        ]


toListLink : Attribution -> Html Msg
toListLink attribution =
    li []
        [ a [ href attribution.link ]
            [ text attribution.name
            , text " by "
            , text attribution.author
            ]
        ]


iconThanksView : Model -> Html Msg
iconThanksView model =
    div [ class "attributions" ]
        [ text "Icons provided by The Noun Project:"
        , ul []
            (List.map toListLink model.attributions)
        ]


infoPane : Model -> Html Msg
infoPane model =
    div [ id "info-pane", class "pane" ]
        [ patreonView model
        , iconThanksView model
        ]


paneContainer : Model -> Html Msg
paneContainer model =
    let
        containerClasses =
            getClassList <| ( .locationFrom model, .locationTo model )
    in
        div [ classList containerClasses ]
            [ searchPane model
            , cardListPane model
            , deckListPane model
            , infoPane model
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


init : ( Model, Cmd Msg )
init =
    ( { locationTo = Nothing
      , locationFrom = Nothing
      , hash = ""
      , cards = []
      , card = getCardId Nothing
      , deck = Dict.empty
      , filterRarity = [ Common, Uncommon, Rare, XRare, URare ]
      , filterSet = [ AT, GL, JL ]

      -- TODO: This isn't flexible
      , filterType = [ Character, Event, Battle ]
      , rarityOpen = False
      , setOpen = False
      , patrons = [ "Matt Smith" ]
      , attributions =
            [ Attribution "Search Icon" "Icon Depot" "https://thenounproject.com/icon/1165408/"
            , Attribution "Cards Icon" "Daniel Solis" "https://thenounproject.com/icon/219514/"
            , Attribution "Deck Icon" "Michael G Brown" "https://thenounproject.com/icon/1156459/"
            , Attribution "Info Icon" "icongeek" "https://thenounproject.com/icon/808461/"
            ]
      }
    , Request.CardList.load
        |> GQL.send CardsLoaded
    )
