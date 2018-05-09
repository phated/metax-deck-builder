module Data.Deck
    exposing
        ( Deck
        , decoder
        , encoder
        , increment
        , decrement
        , empty
        , toList
        , fromList
        , count
        , sum
        , hash
        , toHtml
        )

import Html exposing (Html, div, text, img)
import Html.Attributes exposing (class, id, src)
import Json.Decode as Decode exposing (decodeValue, decodeString, Decoder, Value)
import Json.Encode as Encode exposing (encode)
import Encode
import Component.Card as Card exposing (Card)
import Component.Card.UID as CardUID
import Component.Card.Type as CardType
import Component.Card.Stats as CardStats
import Component.Card.Rank as Rank
import Component.Card.StatType as StatType
import Component.Deck.Slice as DeckSlice exposing (DeckSlice)
import Component.Deck.CharacterSlice as CharacterSlice
import Component.Deck.EventSlice as EventSlice
import Component.Deck.BattleSlice as BattleSlice


type alias Deck =
    { characters : DeckSlice
    , events : DeckSlice
    , strength1 : DeckSlice
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


empty : Deck
empty =
    { characters = DeckSlice.empty
    , events = DeckSlice.empty
    , strength1 = DeckSlice.empty
    , strength2 = DeckSlice.empty
    , strength3 = DeckSlice.empty
    , strength4 = DeckSlice.empty
    , strength5 = DeckSlice.empty
    , strength6 = DeckSlice.empty
    , strength7 = DeckSlice.empty
    , intelligence1 = DeckSlice.empty
    , intelligence2 = DeckSlice.empty
    , intelligence3 = DeckSlice.empty
    , intelligence4 = DeckSlice.empty
    , intelligence5 = DeckSlice.empty
    , intelligence6 = DeckSlice.empty
    , intelligence7 = DeckSlice.empty
    , special1 = DeckSlice.empty
    , special2 = DeckSlice.empty
    , special3 = DeckSlice.empty
    , special4 = DeckSlice.empty
    , special5 = DeckSlice.empty
    , special6 = DeckSlice.empty
    , special7 = DeckSlice.empty
    , multi1 = DeckSlice.empty
    , multi2 = DeckSlice.empty
    , multi3 = DeckSlice.empty
    , multi4 = DeckSlice.empty
    , multi5 = DeckSlice.empty
    , multi6 = DeckSlice.empty
    , multi7 = DeckSlice.empty
    }


toList : Deck -> List ( Card, Int )
toList deck =
    List.concat
        [ DeckSlice.toList deck.characters
        , DeckSlice.toList deck.events
        , DeckSlice.toList deck.strength1
        , DeckSlice.toList deck.strength2
        , DeckSlice.toList deck.strength3
        , DeckSlice.toList deck.strength4
        , DeckSlice.toList deck.strength5
        , DeckSlice.toList deck.strength6
        , DeckSlice.toList deck.strength7
        , DeckSlice.toList deck.intelligence1
        , DeckSlice.toList deck.intelligence2
        , DeckSlice.toList deck.intelligence3
        , DeckSlice.toList deck.intelligence4
        , DeckSlice.toList deck.intelligence5
        , DeckSlice.toList deck.intelligence6
        , DeckSlice.toList deck.intelligence7
        , DeckSlice.toList deck.special1
        , DeckSlice.toList deck.special2
        , DeckSlice.toList deck.special3
        , DeckSlice.toList deck.special4
        , DeckSlice.toList deck.special5
        , DeckSlice.toList deck.special6
        , DeckSlice.toList deck.special7
        , DeckSlice.toList deck.multi1
        , DeckSlice.toList deck.multi2
        , DeckSlice.toList deck.multi3
        , DeckSlice.toList deck.multi4
        , DeckSlice.toList deck.multi5
        , DeckSlice.toList deck.multi6
        , DeckSlice.toList deck.multi7
        ]


fromList : List ( Card, Int ) -> Deck
fromList cards =
    List.foldr (uncurry insert) empty cards


insert : Card -> Int -> Deck -> Deck
insert card count deck =
    case classify card of
        Character ->
            { deck | characters = DeckSlice.insert card count deck.characters }

        Event ->
            { deck | events = DeckSlice.insert card count deck.events }

        Strength1 ->
            { deck | strength1 = DeckSlice.insert card count deck.strength1 }

        Strength2 ->
            { deck | strength2 = DeckSlice.insert card count deck.strength2 }

        Strength3 ->
            { deck | strength3 = DeckSlice.insert card count deck.strength3 }

        Strength4 ->
            { deck | strength4 = DeckSlice.insert card count deck.strength4 }

        Strength5 ->
            { deck | strength5 = DeckSlice.insert card count deck.strength5 }

        Strength6 ->
            { deck | strength6 = DeckSlice.insert card count deck.strength6 }

        Strength7 ->
            { deck | strength7 = DeckSlice.insert card count deck.strength7 }

        Intelligence1 ->
            { deck | intelligence1 = DeckSlice.insert card count deck.intelligence1 }

        Intelligence2 ->
            { deck | intelligence2 = DeckSlice.insert card count deck.intelligence2 }

        Intelligence3 ->
            { deck | intelligence3 = DeckSlice.insert card count deck.intelligence3 }

        Intelligence4 ->
            { deck | intelligence4 = DeckSlice.insert card count deck.intelligence4 }

        Intelligence5 ->
            { deck | intelligence5 = DeckSlice.insert card count deck.intelligence5 }

        Intelligence6 ->
            { deck | intelligence6 = DeckSlice.insert card count deck.intelligence6 }

        Intelligence7 ->
            { deck | intelligence7 = DeckSlice.insert card count deck.intelligence7 }

        Special1 ->
            { deck | special1 = DeckSlice.insert card count deck.special1 }

        Special2 ->
            { deck | special2 = DeckSlice.insert card count deck.special2 }

        Special3 ->
            { deck | special3 = DeckSlice.insert card count deck.special3 }

        Special4 ->
            { deck | special4 = DeckSlice.insert card count deck.special4 }

        Special5 ->
            { deck | special5 = DeckSlice.insert card count deck.special5 }

        Special6 ->
            { deck | special6 = DeckSlice.insert card count deck.special6 }

        Special7 ->
            { deck | special7 = DeckSlice.insert card count deck.special7 }

        Multi1 ->
            { deck | multi1 = DeckSlice.insert card count deck.multi1 }

        Multi2 ->
            { deck | multi2 = DeckSlice.insert card count deck.multi2 }

        Multi3 ->
            { deck | multi3 = DeckSlice.insert card count deck.multi3 }

        Multi4 ->
            { deck | multi4 = DeckSlice.insert card count deck.multi4 }

        Multi5 ->
            { deck | multi5 = DeckSlice.insert card count deck.multi5 }

        Multi6 ->
            { deck | multi6 = DeckSlice.insert card count deck.multi6 }

        Multi7 ->
            { deck | multi7 = DeckSlice.insert card count deck.multi7 }


update : Card -> (Maybe Int -> Maybe Int) -> Deck -> Deck
update card fn deck =
    case classify card of
        Character ->
            { deck | characters = DeckSlice.update card fn deck.characters }

        Event ->
            { deck | events = DeckSlice.update card fn deck.events }

        Strength1 ->
            { deck | strength1 = DeckSlice.update card fn deck.strength1 }

        Strength2 ->
            { deck | strength2 = DeckSlice.update card fn deck.strength2 }

        Strength3 ->
            { deck | strength3 = DeckSlice.update card fn deck.strength3 }

        Strength4 ->
            { deck | strength4 = DeckSlice.update card fn deck.strength4 }

        Strength5 ->
            { deck | strength5 = DeckSlice.update card fn deck.strength5 }

        Strength6 ->
            { deck | strength6 = DeckSlice.update card fn deck.strength6 }

        Strength7 ->
            { deck | strength7 = DeckSlice.update card fn deck.strength7 }

        Intelligence1 ->
            { deck | intelligence1 = DeckSlice.update card fn deck.intelligence1 }

        Intelligence2 ->
            { deck | intelligence2 = DeckSlice.update card fn deck.intelligence2 }

        Intelligence3 ->
            { deck | intelligence3 = DeckSlice.update card fn deck.intelligence3 }

        Intelligence4 ->
            { deck | intelligence4 = DeckSlice.update card fn deck.intelligence4 }

        Intelligence5 ->
            { deck | intelligence5 = DeckSlice.update card fn deck.intelligence5 }

        Intelligence6 ->
            { deck | intelligence6 = DeckSlice.update card fn deck.intelligence6 }

        Intelligence7 ->
            { deck | intelligence7 = DeckSlice.update card fn deck.intelligence7 }

        Special1 ->
            { deck | special1 = DeckSlice.update card fn deck.special1 }

        Special2 ->
            { deck | special2 = DeckSlice.update card fn deck.special2 }

        Special3 ->
            { deck | special3 = DeckSlice.update card fn deck.special3 }

        Special4 ->
            { deck | special4 = DeckSlice.update card fn deck.special4 }

        Special5 ->
            { deck | special5 = DeckSlice.update card fn deck.special5 }

        Special6 ->
            { deck | special6 = DeckSlice.update card fn deck.special6 }

        Special7 ->
            { deck | special7 = DeckSlice.update card fn deck.special7 }

        Multi1 ->
            { deck | multi1 = DeckSlice.update card fn deck.multi1 }

        Multi2 ->
            { deck | multi2 = DeckSlice.update card fn deck.multi2 }

        Multi3 ->
            { deck | multi3 = DeckSlice.update card fn deck.multi3 }

        Multi4 ->
            { deck | multi4 = DeckSlice.update card fn deck.multi4 }

        Multi5 ->
            { deck | multi5 = DeckSlice.update card fn deck.multi5 }

        Multi6 ->
            { deck | multi6 = DeckSlice.update card fn deck.multi6 }

        Multi7 ->
            { deck | multi7 = DeckSlice.update card fn deck.multi7 }


foldr : (Card -> Int -> a -> a) -> a -> Deck -> a
foldr fn result deck =
    let
        flippedFoldr =
            flip (DeckSlice.foldr fn)
    in
        result
            |> flippedFoldr deck.characters
            |> flippedFoldr deck.events
            |> flippedFoldr deck.strength1
            |> flippedFoldr deck.strength2
            |> flippedFoldr deck.strength3
            |> flippedFoldr deck.strength4
            |> flippedFoldr deck.strength5
            |> flippedFoldr deck.strength6
            |> flippedFoldr deck.strength7
            |> flippedFoldr deck.intelligence1
            |> flippedFoldr deck.intelligence2
            |> flippedFoldr deck.intelligence3
            |> flippedFoldr deck.intelligence4
            |> flippedFoldr deck.intelligence5
            |> flippedFoldr deck.intelligence6
            |> flippedFoldr deck.intelligence7
            |> flippedFoldr deck.special1
            |> flippedFoldr deck.special2
            |> flippedFoldr deck.special3
            |> flippedFoldr deck.special4
            |> flippedFoldr deck.special5
            |> flippedFoldr deck.special6
            |> flippedFoldr deck.special7
            |> flippedFoldr deck.multi1
            |> flippedFoldr deck.multi2
            |> flippedFoldr deck.multi3
            |> flippedFoldr deck.multi4
            |> flippedFoldr deck.multi5
            |> flippedFoldr deck.multi6
            |> flippedFoldr deck.multi7


filter : (Card -> Int -> Bool) -> Deck -> Deck
filter fn deck =
    { characters = DeckSlice.filter fn deck.characters
    , events = DeckSlice.filter fn deck.events
    , strength1 = DeckSlice.filter fn deck.strength1
    , strength2 = DeckSlice.filter fn deck.strength2
    , strength3 = DeckSlice.filter fn deck.strength3
    , strength4 = DeckSlice.filter fn deck.strength4
    , strength5 = DeckSlice.filter fn deck.strength5
    , strength6 = DeckSlice.filter fn deck.strength6
    , strength7 = DeckSlice.filter fn deck.strength7
    , intelligence1 = DeckSlice.filter fn deck.intelligence1
    , intelligence2 = DeckSlice.filter fn deck.intelligence2
    , intelligence3 = DeckSlice.filter fn deck.intelligence3
    , intelligence4 = DeckSlice.filter fn deck.intelligence4
    , intelligence5 = DeckSlice.filter fn deck.intelligence5
    , intelligence6 = DeckSlice.filter fn deck.intelligence6
    , intelligence7 = DeckSlice.filter fn deck.intelligence7
    , special1 = DeckSlice.filter fn deck.special1
    , special2 = DeckSlice.filter fn deck.special2
    , special3 = DeckSlice.filter fn deck.special3
    , special4 = DeckSlice.filter fn deck.special4
    , special5 = DeckSlice.filter fn deck.special5
    , special6 = DeckSlice.filter fn deck.special6
    , special7 = DeckSlice.filter fn deck.special7
    , multi1 = DeckSlice.filter fn deck.multi1
    , multi2 = DeckSlice.filter fn deck.multi2
    , multi3 = DeckSlice.filter fn deck.multi3
    , multi4 = DeckSlice.filter fn deck.multi4
    , multi5 = DeckSlice.filter fn deck.multi5
    , multi6 = DeckSlice.filter fn deck.multi6
    , multi7 = DeckSlice.filter fn deck.multi7
    }


count : Card -> Deck -> Int
count card deck =
    case classify card of
        Character ->
            DeckSlice.count card deck.characters

        Event ->
            DeckSlice.count card deck.events

        Strength1 ->
            DeckSlice.count card deck.strength1

        Strength2 ->
            DeckSlice.count card deck.strength2

        Strength3 ->
            DeckSlice.count card deck.strength3

        Strength4 ->
            DeckSlice.count card deck.strength4

        Strength5 ->
            DeckSlice.count card deck.strength5

        Strength6 ->
            DeckSlice.count card deck.strength6

        Strength7 ->
            DeckSlice.count card deck.strength7

        Intelligence1 ->
            DeckSlice.count card deck.intelligence1

        Intelligence2 ->
            DeckSlice.count card deck.intelligence2

        Intelligence3 ->
            DeckSlice.count card deck.intelligence3

        Intelligence4 ->
            DeckSlice.count card deck.intelligence4

        Intelligence5 ->
            DeckSlice.count card deck.intelligence5

        Intelligence6 ->
            DeckSlice.count card deck.intelligence6

        Intelligence7 ->
            DeckSlice.count card deck.intelligence7

        Special1 ->
            DeckSlice.count card deck.special1

        Special2 ->
            DeckSlice.count card deck.special2

        Special3 ->
            DeckSlice.count card deck.special3

        Special4 ->
            DeckSlice.count card deck.special4

        Special5 ->
            DeckSlice.count card deck.special5

        Special6 ->
            DeckSlice.count card deck.special6

        Special7 ->
            DeckSlice.count card deck.special7

        Multi1 ->
            DeckSlice.count card deck.multi1

        Multi2 ->
            DeckSlice.count card deck.multi2

        Multi3 ->
            DeckSlice.count card deck.multi3

        Multi4 ->
            DeckSlice.count card deck.multi4

        Multi5 ->
            DeckSlice.count card deck.multi5

        Multi6 ->
            DeckSlice.count card deck.multi6

        Multi7 ->
            DeckSlice.count card deck.multi7


sum : Deck -> Int
sum deck =
    CharacterSlice.sum deck + EventSlice.sum deck + BattleSlice.sum deck


hash : Deck -> Maybe String
hash deck =
    let
        version =
            0

        encodedVersion =
            Encode.toBase64 version

        result =
            { cardHashes = []
            , checksum = Encode.encodeChecksum encodedVersion
            }

        encodeResult =
            foldr Encode.encodeCard result deck

        encodedDeck =
            encodedVersion ++ String.join "" encodeResult.cardHashes

        base64Checksum =
            Encode.toBase64 <| encodeResult.checksum % 64

        encoded =
            encodedDeck ++ base64Checksum
    in
        -- AiAASAhA
        if List.length encodeResult.cardHashes > 0 then
            Just encoded
        else
            Nothing


increment : Card -> Deck -> Deck
increment card deck =
    update card maybeIncrement deck


decrement : Card -> Deck -> Deck
decrement card deck =
    let
        updatedDeck =
            update card maybeDecrement deck
    in
        filter notZero updatedDeck



-- Encoder/Decoders


decoder : Decoder (List ( String, Int ))
decoder =
    -- TODO: This is a frustrating data type to work with
    Decode.keyValuePairs Decode.int


encoder : Deck -> String
encoder deck =
    let
        toEncoder ( card, count ) =
            ( CardUID.toString card.uid, Encode.int count )
    in
        List.map toEncoder (toList deck)
            |> Encode.object
            |> encode 0


toHtml : Deck -> Html msg
toHtml deck =
    div [ id "deck-list-pane", class "pane" ]
        [ CharacterSlice.toHtml deck
        , EventSlice.toHtml deck
        , BattleSlice.toHtml deck
        ]



-- Utils


type Classifier
    = Character
    | Event
    | Strength1
    | Strength2
    | Strength3
    | Strength4
    | Strength5
    | Strength6
    | Strength7
    | Intelligence1
    | Intelligence2
    | Intelligence3
    | Intelligence4
    | Intelligence5
    | Intelligence6
    | Intelligence7
    | Special1
    | Special2
    | Special3
    | Special4
    | Special5
    | Special6
    | Special7
    | Multi1
    | Multi2
    | Multi3
    | Multi4
    | Multi5
    | Multi6
    | Multi7


classify : Card -> Classifier
classify card =
    case card.card_type of
        CardType.Character ->
            Character

        CardType.Event ->
            Event

        CardType.Battle ->
            case card.stats of
                CardStats.Single { statType, rank } ->
                    case ( statType, Rank.toInt rank ) of
                        ( StatType.Strength, 1 ) ->
                            Strength1

                        ( StatType.Strength, 2 ) ->
                            Strength2

                        ( StatType.Strength, 3 ) ->
                            Strength3

                        ( StatType.Strength, 4 ) ->
                            Strength4

                        ( StatType.Strength, 5 ) ->
                            Strength5

                        ( StatType.Strength, 6 ) ->
                            Strength6

                        ( StatType.Strength, 7 ) ->
                            Strength7

                        ( StatType.Intelligence, 1 ) ->
                            Intelligence1

                        ( StatType.Intelligence, 2 ) ->
                            Intelligence2

                        ( StatType.Intelligence, 3 ) ->
                            Intelligence3

                        ( StatType.Intelligence, 4 ) ->
                            Intelligence4

                        ( StatType.Intelligence, 5 ) ->
                            Intelligence5

                        ( StatType.Intelligence, 6 ) ->
                            Intelligence6

                        ( StatType.Intelligence, 7 ) ->
                            Intelligence7

                        ( StatType.Special, 1 ) ->
                            Special1

                        ( StatType.Special, 2 ) ->
                            Special2

                        ( StatType.Special, 3 ) ->
                            Special3

                        ( StatType.Special, 4 ) ->
                            Special4

                        ( StatType.Special, 5 ) ->
                            Special5

                        ( StatType.Special, 6 ) ->
                            Special6

                        ( StatType.Special, 7 ) ->
                            Special7

                        _ ->
                            Debug.crash "Cannot be a single with rank outside of bounds."

                CardStats.Multi { rank } ->
                    case Rank.toInt rank of
                        1 ->
                            Multi1

                        2 ->
                            Multi2

                        3 ->
                            Multi3

                        4 ->
                            Multi4

                        5 ->
                            Multi5

                        6 ->
                            Multi6

                        7 ->
                            Multi7

                        _ ->
                            Debug.crash "Cannot be a multi with rank outside of bounds."

                _ ->
                    Debug.crash "Cannot reach here."


notZero : Card -> Int -> Bool
notZero _ count =
    count /= 0


maybeIncrement : Maybe Int -> Maybe Int
maybeIncrement value =
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


maybeDecrement : Maybe Int -> Maybe Int
maybeDecrement value =
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
