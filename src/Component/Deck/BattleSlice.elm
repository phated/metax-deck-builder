module Component.Deck.BattleSlice
    exposing
        ( sum
        , toHtml
        )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Helpers
import Component.Deck.Slice as DeckSlice exposing (DeckSlice)


type alias BattleSlice a =
    { a
        | strength1 : DeckSlice
        , strength2 : DeckSlice
        , strength3 : DeckSlice
        , strength4 : DeckSlice
        , strength5 : DeckSlice
        , strength6 : DeckSlice
        , strength7 : DeckSlice
        , intelligence1 : DeckSlice
        , intelligence2 : DeckSlice
        , intelligence3 : DeckSlice
        , intelligence4 : DeckSlice
        , intelligence5 : DeckSlice
        , intelligence6 : DeckSlice
        , intelligence7 : DeckSlice
        , special1 : DeckSlice
        , special2 : DeckSlice
        , special3 : DeckSlice
        , special4 : DeckSlice
        , special5 : DeckSlice
        , special6 : DeckSlice
        , special7 : DeckSlice
        , multi1 : DeckSlice
        , multi2 : DeckSlice
        , multi3 : DeckSlice
        , multi4 : DeckSlice
        , multi5 : DeckSlice
        , multi6 : DeckSlice
        , multi7 : DeckSlice
    }


isEmpty : BattleSlice a -> Bool
isEmpty deck =
    DeckSlice.isEmpty deck.strength1
        && DeckSlice.isEmpty deck.strength2
        && DeckSlice.isEmpty deck.strength3
        && DeckSlice.isEmpty deck.strength4
        && DeckSlice.isEmpty deck.strength5
        && DeckSlice.isEmpty deck.strength6
        && DeckSlice.isEmpty deck.strength7
        && DeckSlice.isEmpty deck.intelligence1
        && DeckSlice.isEmpty deck.intelligence2
        && DeckSlice.isEmpty deck.intelligence3
        && DeckSlice.isEmpty deck.intelligence4
        && DeckSlice.isEmpty deck.intelligence5
        && DeckSlice.isEmpty deck.intelligence6
        && DeckSlice.isEmpty deck.intelligence7
        && DeckSlice.isEmpty deck.special1
        && DeckSlice.isEmpty deck.special2
        && DeckSlice.isEmpty deck.special3
        && DeckSlice.isEmpty deck.special4
        && DeckSlice.isEmpty deck.special5
        && DeckSlice.isEmpty deck.special6
        && DeckSlice.isEmpty deck.special7
        && DeckSlice.isEmpty deck.multi1
        && DeckSlice.isEmpty deck.multi2
        && DeckSlice.isEmpty deck.multi3
        && DeckSlice.isEmpty deck.multi4
        && DeckSlice.isEmpty deck.multi5
        && DeckSlice.isEmpty deck.multi6
        && DeckSlice.isEmpty deck.multi7


sum : BattleSlice a -> Int
sum deck =
    DeckSlice.sum deck.strength1
        + DeckSlice.sum deck.strength2
        + DeckSlice.sum deck.strength3
        + DeckSlice.sum deck.strength4
        + DeckSlice.sum deck.strength5
        + DeckSlice.sum deck.strength6
        + DeckSlice.sum deck.strength7
        + DeckSlice.sum deck.intelligence1
        + DeckSlice.sum deck.intelligence2
        + DeckSlice.sum deck.intelligence3
        + DeckSlice.sum deck.intelligence4
        + DeckSlice.sum deck.intelligence5
        + DeckSlice.sum deck.intelligence6
        + DeckSlice.sum deck.intelligence7
        + DeckSlice.sum deck.special1
        + DeckSlice.sum deck.special2
        + DeckSlice.sum deck.special3
        + DeckSlice.sum deck.special4
        + DeckSlice.sum deck.special5
        + DeckSlice.sum deck.special6
        + DeckSlice.sum deck.special7
        + DeckSlice.sum deck.multi1
        + DeckSlice.sum deck.multi2
        + DeckSlice.sum deck.multi3
        + DeckSlice.sum deck.multi4
        + DeckSlice.sum deck.multi5
        + DeckSlice.sum deck.multi6
        + DeckSlice.sum deck.multi7


toHtml : BattleSlice a -> Html msg
toHtml deck =
    case isEmpty deck of
        True ->
            Html.Helpers.nothing

        False ->
            let
                headerText =
                    sum deck
                        |> toString
                        |> (++) "Battle Cards ("
                        |> flip (++) ")"

                header =
                    div [ class "deck-section-header" ] [ text headerText ]
            in
                div [ class "deck-section" ]
                    [ header
                    , statRankSection "Strength - Rank 1" deck.strength1
                    , statRankSection "Strength - Rank 2" deck.strength2
                    , statRankSection "Strength - Rank 3" deck.strength3
                    , statRankSection "Strength - Rank 4" deck.strength4
                    , statRankSection "Strength - Rank 5" deck.strength5
                    , statRankSection "Strength - Rank 6" deck.strength6
                    , statRankSection "Strength - Rank 7" deck.strength7
                    , statRankSection "Intelligence - Rank 1" deck.intelligence1
                    , statRankSection "Intelligence - Rank 2" deck.intelligence2
                    , statRankSection "Intelligence - Rank 3" deck.intelligence3
                    , statRankSection "Intelligence - Rank 4" deck.intelligence4
                    , statRankSection "Intelligence - Rank 5" deck.intelligence5
                    , statRankSection "Intelligence - Rank 6" deck.intelligence6
                    , statRankSection "Intelligence - Rank 7" deck.intelligence7
                    , statRankSection "Special - Rank 1" deck.special1
                    , statRankSection "Special - Rank 2" deck.special2
                    , statRankSection "Special - Rank 3" deck.special3
                    , statRankSection "Special - Rank 4" deck.special4
                    , statRankSection "Special - Rank 5" deck.special5
                    , statRankSection "Special - Rank 6" deck.special6
                    , statRankSection "Special - Rank 7" deck.special7
                    , statRankSection "Multi - Rank 1" deck.multi1
                    , statRankSection "Multi - Rank 2" deck.multi2
                    , statRankSection "Multi - Rank 3" deck.multi3
                    , statRankSection "Multi - Rank 4" deck.multi4
                    , statRankSection "Multi - Rank 5" deck.multi5
                    , statRankSection "Multi - Rank 6" deck.multi6
                    , statRankSection "Multi - Rank 7" deck.multi7
                    ]



-- Internals


statRankSection : String -> DeckSlice -> Html msg
statRankSection title deck =
    case DeckSlice.isEmpty deck of
        True ->
            Html.Helpers.nothing

        False ->
            let
                sum =
                    DeckSlice.sum deck

                warning =
                    warningMessage sum

                headerText =
                    sum
                        |> toString
                        |> (++) " ("
                        |> (++) title
                        |> flip (++) ")"

                header =
                    case warning of
                        Just warning ->
                            div [ class "deck-section-sub-header with-warning" ] [ text headerText, warning ]

                        Nothing ->
                            div [ class "deck-section-sub-header" ] [ text headerText ]
            in
                div []
                    [ header
                    , DeckSlice.toHtml deck
                    ]


warningMessage : Int -> Maybe (Html msg)
warningMessage count =
    if count > 3 then
        Just <| div [ class "warning-message" ] [ text "You have too many Battle Cards at this Type/Rank" ]
    else
        Nothing
