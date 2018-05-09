module Component.Deck.Slice
    exposing
        ( DeckSlice
        , empty
        , size
        , isEmpty
        , foldr
        , filter
        , map
        , insert
        , update
        , count
        , sum
        , toList
        , toHtml
        )

import Html exposing (Html, div, text, img)
import Html.Attributes exposing (id, class, src)
import Regex
import Avl.Dict as Dict exposing (Dict)
import Component.Card as Card exposing (Card)
import Component.Card.UID as CardUID


type alias DeckSlice =
    Dict Card Int


empty : DeckSlice
empty =
    Dict.empty


size : DeckSlice -> Int
size deck =
    Dict.size deck


isEmpty : DeckSlice -> Bool
isEmpty deck =
    size deck == 0


toList : DeckSlice -> List ( Card, Int )
toList deck =
    Dict.toList deck


foldr : (Card -> Int -> b -> b) -> b -> DeckSlice -> b
foldr fn result deck =
    Dict.foldr fn result deck


filter : (Card -> Int -> Bool) -> DeckSlice -> DeckSlice
filter fn deck =
    Dict.filter Card.order fn deck


map : (Card -> Int -> b) -> DeckSlice -> List b
map fn deck =
    let
        append card count result =
            (fn card count) :: result
    in
        foldr append [] deck


insert : Card -> Int -> DeckSlice -> DeckSlice
insert card count deck =
    Dict.insert Card.order card count deck


update : Card -> (Maybe Int -> Maybe Int) -> DeckSlice -> DeckSlice
update card fn deck =
    Dict.update Card.order card fn deck


count : Card -> DeckSlice -> Int
count card deck =
    Dict.get Card.order card deck
        |> Maybe.withDefault 0


sum : DeckSlice -> Int
sum deck =
    Dict.foldr (\_ count result -> result + count) 0 deck


toHtml : DeckSlice -> Html msg
toHtml deck =
    div [ class "list-item-grid" ] (map cardView deck)



-- Internals


cardView : Card -> Int -> Html msg
cardView card count =
    div
        [ id ("deck_" ++ (CardUID.toString card.uid))
        , class "list-item"
        ]
        [ div [ class "card-contents" ]
            [ div [ class "card-image-container" ]
                [ -- [ linkTo (Route.Card (CardUID.toString card.uid))
                  div [ class "card-thumbnail" ]
                    [ img [ src (Regex.replace Regex.All (Regex.regex "/images/") (\_ -> "/thumbnails/") card.image_url) ] []

                    --     , previewBanner card
                    ]
                , div [ class "card-number" ] [ text (CardUID.toString card.uid) ]
                ]
            , Card.toHtml card
            ]

        -- , stepper ( card, count )
        ]
