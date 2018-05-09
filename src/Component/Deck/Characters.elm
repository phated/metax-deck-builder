module Component.Deck.Characters
    exposing
        ( Characters
        , empty
        , foldr
        , filter
        , insert
        , update
        , count
        , sum
        , toList
        , toHtml
        )

import Html exposing (Html, div, text)
import Html.Attributes exposing (id, class)
import Avl.Dict as Dict exposing (Dict)
import Component.Card as Card exposing (Card)
import Component.Card.UID as CardUID


type alias Characters =
    Dict Card Int


empty : Characters
empty =
    Dict.empty


toList : { a | characters : Characters } -> List ( Card, Int )
toList deck =
    Dict.toList deck.characters


foldr : (Card -> Int -> b -> b) -> b -> { a | characters : Characters } -> b
foldr fn result deck =
    Dict.foldr fn result deck.characters


filter : (Card -> Int -> Bool) -> { a | characters : Characters } -> { a | characters : Characters }
filter fn deck =
    { deck | characters = Dict.filter Card.order fn deck.characters }


map : (Card -> Int -> b) -> { a | characters : Characters } -> List b
map fn deck =
    let
        append card count result =
            (fn card count) :: result
    in
        foldr append [] deck


insert : Card -> Int -> { a | characters : Characters } -> { a | characters : Characters }
insert card count deck =
    { deck | characters = Dict.insert Card.order card count deck.characters }


update : Card -> (Maybe Int -> Maybe Int) -> { a | characters : Characters } -> { a | characters : Characters }
update card fn deck =
    { deck | characters = Dict.update Card.order card fn deck.characters }


count : Card -> { a | characters : Characters } -> Int
count card deck =
    Dict.get Card.order card deck.characters
        |> Maybe.withDefault 0


sum : { a | characters : Characters } -> Int
sum deck =
    Dict.foldr (\_ count result -> result + count) 0 deck.characters


toHtml : { a | characters : Characters } -> Html msg
toHtml deck =
    let
        cards =
            map cardView deck

        title =
            sum deck
                |> toString
                |> (++) "Characters ("
                |> flip (++) ")"

        children =
            (div [ class "list-item-header" ] [ text title ])
                :: cards
    in
        div [] children



-- Internals


cardView : Card -> Int -> Html msg
cardView card count =
    div
        [ id ("deck_" ++ (CardUID.toString card.uid))
        , class "list-item"
        ]
        [ div [ class "card-image-container" ] []

        -- [ linkTo (Route.Card (CardUID.toString card.uid))
        --     [ class "card-thumbnail" ]
        --     [ img [ src (Regex.replace Regex.All (Regex.regex "/images/") (\_ -> "/thumbnails/") card.image_url) ] []
        --     , previewBanner card
        --     ]
        -- , div [ class "card-number" ] [ text (CardUID.toString card.uid) ]
        -- ]
        , Card.toHtml card

        -- , stepper ( card, count )
        ]
