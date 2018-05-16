module Component.Deck.CharacterSlice
    exposing
        ( sum
        , toHtml
        )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Helpers
import Component.Card exposing (Card)
import Component.Deck.Slice as DeckSlice exposing (DeckSlice)


sum : { a | characters : DeckSlice } -> Int
sum deck =
    DeckSlice.sum deck.characters


toHtml : (Card -> Int -> Html msg) -> { a | characters : DeckSlice } -> Html msg
toHtml renderChild deck =
    case DeckSlice.isEmpty deck.characters of
        True ->
            Html.Helpers.nothing

        False ->
            let
                cards =
                    DeckSlice.toHtml renderChild deck.characters

                headerText =
                    DeckSlice.sum deck.characters
                        |> toString
                        |> (++) "Characters ("
                        |> flip (++) ")"

                header =
                    div [ class "deck-section-header" ] [ text headerText ]
            in
                div [ class "deck-section" ]
                    [ header
                    , cards
                    ]
