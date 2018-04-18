module Main exposing (Model, Msg, update, view, subscriptions, init)

import Http
import Html exposing (header, nav, div, img, text, button, a, span, label, input, li, ul, br, Html)
import Html.Attributes exposing (href, class, classList, id, src, alt, disabled, type_, placeholder, value, checked)
import Html.Events exposing (onClick, onCheck)
import Html.Helpers
import Navigation exposing (Location)
import Dict exposing (Dict)
import Regex exposing (regex, contains, replace, Regex)
import Data.Card as Card exposing (Card)
import Data.CardList as CardList exposing (CardList)
import Data.CardType as CardType exposing (CardType(..))
import Data.CardSet as CardSet exposing (CardSet(JL, GL, AT))
import Data.CardEffect as CardEffect exposing (CardEffect)
import Data.CardRarity as CardRarity exposing (CardRarity(Common, Uncommon, Rare, XRare, URare, Promo, Starter))
import Data.CardStatList exposing (CardStatList)
import Data.CardStat as CardStat exposing (CardStat)
import Data.Deck as Deck exposing (Deck)
import Data.Attribution as Attribution exposing (Attribution)
import Data.BattleType as BattleType exposing (BattleType)
import Data.CardUID as CardUID exposing (CardUID)
import Request.Deck
import Request.CardList
import Util exposing (onNavigate)
import Route exposing (fromLocation, toHref, Route)
import Ports exposing (onSessionLoaded, loadSession)
import GraphQl as Gql
import RouteUrl exposing (RouteUrlProgram, UrlChange, HistoryEntry(NewEntry, ModifyEntry))
import Data.Filters as Filters exposing (Filters, Filter(FilterRarity, FilterSet, FilterUID))
import Fork.QueryString as QueryString


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


type alias Model =
    { locationTo : Maybe Route
    , locationFrom : Maybe Route
    , cards : CardList
    , deck : Deck
    , importedDeck : List ( String, Int )
    , filters : Filters
    , rarityOpen : Bool
    , setOpen : Bool
    , patrons : List String
    , attributions : List Attribution
    , isLoading : Bool

    -- TODO: This is storing the card for individual preview until I find a better place to store it
    , card : Maybe Card
    , cardLoading : Bool
    }


type Msg
    = SetRoute Route
    | CardsLoaded (Result Http.Error CardList)
    | LoadDeckCards (Result Http.Error CardList)
    | Decrement Card
    | Increment Card
    | ExportDeck
    | ImportDeck (List ( String, Int ))
    | ToggleOpenRarity
    | ToggleOpenSet
    | AddFilter Filter
    | RemoveFilter Filter
    | UpdateFilters Filters
    | Search
    | PreloadCards
    | CardsPreloaded (Result Http.Error CardList)
    | LoadCard (Result Http.Error Card)


delta2url : Model -> Model -> Maybe UrlChange
delta2url prevModel nextModel =
    let
        querystring =
            Filters.toQueryString nextModel.filters
                |> QueryString.addBy Deck.hash "deck" nextModel.deck
                |> QueryString.render
    in
        case nextModel.locationTo of
            Just route ->
                Just
                    { entry = NewEntry
                    , url = (Route.toHref route) ++ querystring
                    }

            Nothing ->
                Just
                    { entry = ModifyEntry
                    , url = querystring
                    }


location2messages : Location -> List Msg
location2messages location =
    let
        qs =
            QueryString.parse location.search

        filters =
            Filters.fromQueryString qs

        route =
            fromLocation location
    in
        case route of
            Just route ->
                -- TODO: Are these executed in order?
                [ SetRoute route
                , {- TODO: If init had the location, I could avoid this on initialization -} UpdateFilters filters
                , PreloadCards
                ]

            Nothing ->
                []


importedToDeckItem : CardList -> ( String, Int ) -> Maybe ( Card, Int )
importedToDeckItem cards ( uid, count ) =
    let
        card =
            lookup cards uid
    in
        case card of
            Just card ->
                Just ( card, count )

            Nothing ->
                Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute route ->
            let
                ( loading, cmd ) =
                    case route of
                        Route.Card uid ->
                            ( True
                            , CardUID.fromString uid
                                |> Card.query
                                |> Card.load
                                |> Gql.send LoadCard
                            )

                        _ ->
                            ( False, Cmd.none )
            in
                ( { model
                    | locationFrom = model.locationTo
                    , locationTo = Just route
                    , card = Nothing
                    , cardLoading = loading
                  }
                , cmd
                )

        LoadCard (Ok card) ->
            ( { model | card = Just card, cardLoading = False }, Cmd.none )

        LoadCard (Err err) ->
            let
                test =
                    Debug.log "err" err
            in
                ( { model | cardLoading = False }, Cmd.none )

        LoadDeckCards (Ok cards) ->
            -- TODO: It feels really bad to to this mapping from UID to actual Card. Maybe this should be stored?
            let
                deck =
                    model.importedDeck
                        |> List.filterMap (importedToDeckItem cards)
                        |> Deck.fromList
            in
                ( { model | deck = deck, importedDeck = [] }, Cmd.none )

        LoadDeckCards (Err err) ->
            let
                test =
                    Debug.log "err" err
            in
                ( model, Cmd.none )

        ImportDeck imported ->
            let
                uids =
                    Filters.fromList <| List.map (\( uid, _ ) -> FilterUID (CardUID.fromString uid)) imported
            in
                ( { model | importedDeck = imported }, buildQuery uids |> Gql.send LoadDeckCards )

        CardsLoaded (Ok cards) ->
            ( { model | isLoading = False, cards = (CardList.sort cards), locationFrom = model.locationTo, locationTo = Just Route.Home }, Cmd.none )

        CardsLoaded (Err err) ->
            let
                test =
                    Debug.log "err" err
            in
                ( { model | isLoading = False }, Cmd.none )

        CardsPreloaded (Ok cards) ->
            ( { model | isLoading = False, cards = (CardList.sort cards) }, Cmd.none )

        CardsPreloaded (Err err) ->
            let
                test =
                    Debug.log "err" err
            in
                ( { model | isLoading = False }, Cmd.none )

        Increment card ->
            let
                deck =
                    Deck.increment card model.deck
            in
                ( { model | deck = deck }, Cmd.none )

        Decrement card ->
            let
                deck =
                    Deck.decrement card model.deck
            in
                ( { model | deck = deck }, Cmd.none )

        ExportDeck ->
            ( model, Request.Deck.export model.deck )

        AddFilter filter ->
            let
                filters =
                    Filters.add filter model.filters
            in
                ( { model | filters = filters }, Cmd.none )

        RemoveFilter filter ->
            let
                filters =
                    Filters.remove filter model.filters
            in
                ( { model | filters = filters }, Cmd.none )

        UpdateFilters filters ->
            let
                nextFilters =
                    Filters.union filters model.filters
            in
                ( { model | filters = nextFilters }, Cmd.none )

        Search ->
            ( { model | isLoading = True }, buildQuery model.filters |> Gql.send CardsLoaded )

        PreloadCards ->
            ( { model | isLoading = True }, buildQuery model.filters |> Gql.send CardsPreloaded )

        ToggleOpenRarity ->
            ( { model | rarityOpen = not model.rarityOpen }, Cmd.none )

        ToggleOpenSet ->
            ( { model | setOpen = not model.setOpen }, Cmd.none )


view : Model -> Html Msg
view model =
    applicationShell model


buildQuery : Filters -> Gql.Request Gql.Query Gql.Named CardList
buildQuery filters =
    Filters.applyFilters filters Request.CardList.allCards
        |> Request.CardList.load


linkTo : Route -> (List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg)
linkTo route =
    let
        linkAttrs =
            [ href <| toHref route
            , onNavigate (SetRoute route)
            ]
    in
        (\attrs contents -> a (List.append attrs linkAttrs) contents)


logo : String -> Html Msg
logo title =
    linkTo Route.Home
        [ class "navitem logo" ]
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
            Html.Helpers.nothing

        Just Route.Deck ->
            downloadButton

        Just (Route.Card _) ->
            Html.Helpers.nothing

        Just Route.Search ->
            Html.Helpers.nothing

        Just Route.Info ->
            Html.Helpers.nothing

        Nothing ->
            Html.Helpers.nothing


navbarTop : Model -> Html Msg
navbarTop model =
    header [ class "topbar" ]
        [ logo "MetaX DB"
        , getNavbarIcon <| .locationTo model
        ]


decklistText : Model -> Html Msg
decklistText model =
    Deck.sum model.deck
        |> toString
        |> text


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


cardListPane : Model -> Html Msg
cardListPane model =
    div
        [ id "card-list-pane"
        , class "pane"
        ]
        (List.map (cardView model) model.cards)


stepper : ( Card, Int ) -> Html Msg
stepper ( card, count ) =
    let
        decrementDisabled =
            (count == 0)

        incrementDisabled =
            (count == 3)
    in
        div [ class "stepper-container" ]
            [ button [ class "stepper-button stepper-decrement ripple", disabled decrementDisabled, onClick (Decrement card) ] [ text "-" ]
            , button [ class "stepper-button stepper-increment ripple", disabled incrementDisabled, onClick (Increment card) ] [ text "+" ]
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


cardTrait : String -> String
cardTrait trait =
    if trait == "" then
        ""
    else
        trait


cardText : Card -> Html Msg
cardText card =
    div [ class "card-text" ]
        [ div [ class "card-title" ]
            [ text card.title
            , text <| Maybe.withDefault "" <| Maybe.map (\s -> " - " ++ s) card.subtitle
            , text <| toBattleCardRank card
            ]
        , div [ class "card-trait" ] [ text <| cardTrait card.trait ]
        , CardEffect.toHtml card.effect
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
            Html.Helpers.nothing


cardDetails : Card -> Html Msg
cardDetails card =
    div [ class "card-details" ]
        [ div [ class "card-image-container" ]
            [ linkTo (Route.Card (CardUID.toString card.uid))
                [ class "card-thumbnail" ]
                [ img [ src (replace Regex.All (regex "/images/") (\_ -> "/thumbnails/") card.image_url) ] []
                , previewBanner card
                ]
            , div [ class "card-number" ] [ text (CardUID.toString card.uid) ]
            ]
        , cardText card
        , cardStats card
        ]


cardView : Model -> Card -> Html Msg
cardView model card =
    let
        count =
            Deck.count card model.deck
    in
        div
            [ id (CardUID.toString card.uid)
            , class "list-item"
            ]
            [ cardDetails card
            , stepper ( card, count )
            ]


idMatches : String -> Card -> Bool
idMatches cardId card =
    (CardUID.toString card.uid) == cardId


lookup : CardList -> String -> Maybe Card
lookup cards cardId =
    List.filter (idMatches cardId) cards
        |> List.head


sectionHeader : String -> Int -> List (Html Msg)
sectionHeader title count =
    List.singleton (div [ class "list-item-header" ] [ text <| title ++ " (" ++ (toString count) ++ ")" ])


sectionSubHeader : Html Msg -> List (Html Msg)
sectionSubHeader content =
    List.singleton (div [ class "list-item-sub-header" ] [ content ])


sum : List ( Card, Int ) -> Int
sum cards =
    (List.sum (List.map Tuple.second cards))


charactersView : List ( Card, Int ) -> List (Html Msg)
charactersView characters =
    if List.length characters > 0 then
        List.concat
            [ sectionHeader "Characters" (sum characters)
            , (List.map deckCardView characters)
            ]
    else
        []


eventsView : List ( Card, Int ) -> List (Html Msg)
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


battleCardSubSection : String -> List ( Card, Int ) -> List (Html Msg)
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
    { strength : Dict Int (List ( Card, Int ))
    , intelligence : Dict Int (List ( Card, Int ))
    , special : Dict Int (List ( Card, Int ))
    , multi : Dict Int (List ( Card, Int ))
    }


addToRank : ( Card, Int ) -> Maybe (List ( Card, Int )) -> Maybe (List ( Card, Int ))
addToRank item list =
    case list of
        Just list ->
            Just (item :: list)

        Nothing ->
            Just [ item ]


groupBattleCards : ( Card, Int ) -> BattleCardGroups -> BattleCardGroups
groupBattleCards ( card, count ) result =
    case card.card_type of
        Battle ->
            case BattleType.toBattleType card.stats of
                Just (BattleType.Strength rank) ->
                    { result | strength = Dict.update rank (addToRank ( card, count )) result.strength }

                Just (BattleType.Intelligence rank) ->
                    { result | intelligence = Dict.update rank (addToRank ( card, count )) result.intelligence }

                Just (BattleType.Special rank) ->
                    { result | special = Dict.update rank (addToRank ( card, count )) result.special }

                Just (BattleType.Multi rank) ->
                    { result | multi = Dict.update rank (addToRank ( card, count )) result.multi }

                Nothing ->
                    result

        Character ->
            result

        Event ->
            result


toRows : String -> Int -> List ( Card, Int ) -> List (Html Msg) -> List (Html Msg)
toRows title rank cards result =
    List.append result (battleCardSubSection (title ++ " - Rank " ++ (toString rank)) cards)


battleCardView : List ( Card, Int ) -> List (Html Msg)
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
    { characters : List ( Card, Int )
    , events : List ( Card, Int )
    , battle : List ( Card, Int )
    }


groupTypes : Card -> Int -> DeckGroups -> DeckGroups
groupTypes card count result =
    case card.card_type of
        Character ->
            { result | characters = ( card, count ) :: result.characters }

        Event ->
            { result | events = ( card, count ) :: result.events }

        Battle ->
            { result | battle = ( card, count ) :: result.battle }


deckSectionView : Deck -> List (Html Msg)
deckSectionView deck =
    let
        rows =
            Deck.foldl groupTypes (DeckGroups [] [] []) deck
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


deckCardView : ( Card, Int ) -> Html Msg
deckCardView ( card, count ) =
    div
        [ id ("deck_" ++ (CardUID.toString card.uid))
        , class "list-item"
        ]
        [ cardDetails card
        , stepper ( card, count )
        ]


deckListPane : Model -> Html Msg
deckListPane model =
    div
        [ id "deck-list-pane"
        , class "pane"
        ]
        (deckSectionView model.deck)


previewedBy : Card -> Html Msg
previewedBy card =
    case card.preview of
        Just preview ->
            div [ class "preview-banner previewed-by" ] [ text "Previewed By: ", a [ href preview.previewUrl ] [ text preview.previewer ] ]

        Nothing ->
            Html.Helpers.nothing


largeImg : Card -> Html Msg
largeImg card =
    div [ class "card-full-wrapper" ]
        [ img [ class "card-full", src card.image_url ] []
        , previewedBy card
        ]


cardPane : Model -> Html Msg
cardPane model =
    case .locationTo model of
        Just (Route.Card _) ->
            case .card model of
                Just card ->
                    div
                        [ id "card-pane"
                        , class "pane"
                        ]
                        [ div [ class "card-full-container" ]
                            [ largeImg card
                            , div [ class "card-details" ]
                                [ cardText card
                                , cardStats card
                                ]
                            , stepper ( card, Deck.count card model.deck )
                            ]
                        ]

                Nothing ->
                    div
                        [ id "card-pane"
                        , class "pane align-center"
                        ]
                        [ text
                            (if model.cardLoading then
                                "Loading..."
                             else
                                "Card not found"
                            )
                        ]

        _ ->
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


toggleFilter : Filter -> Bool -> Msg
toggleFilter filter isChecked =
    if isChecked then
        AddFilter filter
    else
        RemoveFilter filter


checkbox : String -> Bool -> (Bool -> Msg) -> Html Msg
checkbox label_ isChecked msg =
    label [ class "checkbox" ]
        [ input [ type_ "checkbox", checked isChecked, onCheck msg ] []
        , text label_
        ]


searchPane : Model -> Html Msg
searchPane model =
    div
        [ id "search-pane"
        , class "pane"
        ]
        [ input [ type_ "search", placeholder "Search", class "search-box", value (Filters.toString model.filters) ] []
        , div [ class "help-text" ] [ text "Need help?" ]
        , div [ classList [ ( "option-container", True ), ( "is-open", model.rarityOpen ) ] ]
            [ div [ class "option-title", onClick ToggleOpenRarity ] [ text "Rarity" ]
            , div [ class "option-body" ]
                [ checkbox "Common" (Filters.member (FilterRarity Common) model.filters) (toggleFilter <| FilterRarity Common)
                , checkbox "Uncommon" (Filters.member (FilterRarity Uncommon) model.filters) (toggleFilter <| FilterRarity Uncommon)
                , checkbox "Rare" (Filters.member (FilterRarity Rare) model.filters) (toggleFilter <| FilterRarity Rare)
                , checkbox "XR" (Filters.member (FilterRarity XRare) model.filters) (toggleFilter <| FilterRarity XRare)
                , checkbox "UR" (Filters.member (FilterRarity URare) model.filters) (toggleFilter <| FilterRarity URare)
                , checkbox "Promo" (Filters.member (FilterRarity Promo) model.filters) (toggleFilter <| FilterRarity Promo)
                , checkbox "Starter" (Filters.member (FilterRarity Starter) model.filters) (toggleFilter <| FilterRarity Starter)
                ]
            ]
        , div [ classList [ ( "option-container", True ), ( "is-open", model.setOpen ) ] ]
            [ div [ class "option-title", onClick ToggleOpenSet ] [ text "Set" ]
            , div [ class "option-body" ]
                [ checkbox "Attack on Titan" (Filters.member (FilterSet AT) model.filters) (toggleFilter <| FilterSet AT)
                , checkbox "Green Lantern" (Filters.member (FilterSet GL) model.filters) (toggleFilter <| FilterSet GL)
                , checkbox "Justice Leauge" (Filters.member (FilterSet JL) model.filters) (toggleFilter <| FilterSet JL)
                ]
            ]
        , button
            [ class "search-button"
            , onClick Search
            , disabled model.isLoading
            ]
            [ text
                (if model.isLoading then
                    "Searching..."
                 else
                    "Search"
                )
            ]
        ]


patreonView : Model -> Html Msg
patreonView model =
    div [ class "patreon" ]
        [ div [] [ text "Our work is supported by our Patrons on Patreon." ]
        , div []
            [ text "Extra "
            , img [ class "card-stat-icon", src "/icons/special.png" ] []
            , text " thanks to our Rank 5+ Patrons: "
            , span [ class "patreon-rank-5" ] (List.map text model.patrons)
            ]
        , div [ class "patreon" ] [ text "For development updates and extra features, subscribe to us on Patreon!" ]
        , a [ class "patreon-link", href "https://www.patreon.com/metaxdb" ] [ img [ src "/icons/patron.png", alt "Become a Patron" ] [] ]
        ]


iconThanksView : Model -> Html Msg
iconThanksView model =
    div [ class "attributions" ]
        [ text "Icons provided by The Noun Project:"
        , ul []
            (List.map Attribution.toHtml model.attributions)
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


noSaveWarning : Html Msg
noSaveWarning =
    div [ class "save-warning" ]
        [ text "We have disabled auto-saving of decklists."
        , br [] []
        , text "Be sure to save/bookmark your deck hash."
        ]


applicationShell : Model -> Html Msg
applicationShell model =
    div [ class "pane-root" ]
        [ navbarTop model
        , noSaveWarning
        , paneContainer model
        , navbarBottom model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ImportDeck (onSessionLoaded Deck.decoder)


init : ( Model, Cmd Msg )
init =
    let
        model =
            { locationTo = Nothing
            , locationFrom = Nothing
            , cards = []
            , deck = Deck.empty
            , importedDeck = []
            , filters = Filters.empty
            , rarityOpen = False
            , setOpen = False
            , patrons = [ "Matt Smith" ]
            , attributions =
                [ Attribution "Search Icon" "Icon Depot" "https://thenounproject.com/icon/1165408/"
                , Attribution "Cards Icon" "Daniel Solis" "https://thenounproject.com/icon/219514/"
                , Attribution "Deck Icon" "Michael G Brown" "https://thenounproject.com/icon/1156459/"
                , Attribution "Info Icon" "icongeek" "https://thenounproject.com/icon/808461/"
                ]
            , isLoading = False
            , card = Nothing
            , cardLoading = False
            }
    in
        ( model, loadSession "" )
