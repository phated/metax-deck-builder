module Component.Deck.EventSlice
    exposing
        ( sum
        , toHtml
        )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Helpers
import Component.Deck.Slice as DeckSlice exposing (DeckSlice)


sum : { a | events : DeckSlice } -> Int
sum deck =
    DeckSlice.sum deck.events


toHtml : { a | events : DeckSlice } -> Html msg
toHtml deck =
    case DeckSlice.isEmpty deck.events of
        True ->
            Html.Helpers.nothing

        False ->
            let
                cards =
                    DeckSlice.toHtml deck.events

                headerText =
                    DeckSlice.sum deck.events
                        |> toString
                        |> (++) "Events ("
                        |> flip (++) ")"

                header =
                    div [ class "deck-section-header" ] [ text headerText ]
            in
                div [ class "deck-section" ]
                    [ header
                    , cards
                    ]
