module Component.Deck.Events
    exposing
        ( Events
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

import Html exposing (Html, div)
import Html.Attributes exposing (id, class)
import Avl.Dict as Dict exposing (Dict)
import Component.Card as Card exposing (Card)
import Component.Card.UID as CardUID


type alias Events =
    Dict Card Int


empty : Events
empty =
    Dict.empty


toList : { a | events : Events } -> List ( Card, Int )
toList deck =
    Dict.toList deck.events


foldr : (Card -> Int -> b -> b) -> b -> { a | events : Events } -> b
foldr fn result deck =
    Dict.foldr fn result deck.events


filter : (Card -> Int -> Bool) -> { a | events : Events } -> { a | events : Events }
filter fn deck =
    { deck | events = Dict.filter Card.order fn deck.events }


map : (Card -> Int -> b) -> { a | events : Events } -> List b
map fn deck =
    let
        append card count result =
            (fn card count) :: result
    in
        foldr append [] deck


insert : Card -> Int -> { a | events : Events } -> { a | events : Events }
insert card count deck =
    { deck | events = Dict.insert Card.order card count deck.events }


update : Card -> (Maybe Int -> Maybe Int) -> { a | events : Events } -> { a | events : Events }
update card fn deck =
    { deck | events = Dict.update Card.order card fn deck.events }


count : Card -> { a | events : Events } -> Int
count card deck =
    Dict.get Card.order card deck.events
        |> Maybe.withDefault 0


sum : { a | events : Events } -> Int
sum deck =
    Dict.foldr (\_ count result -> result + count) 0 deck.events


toHtml : { a | events : Events } -> Html msg
toHtml deck =
    div [] (map cardView deck)



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
