# Modules
- [Component.CardEffect](#componentcardeffect)

# Component.CardEffect
- [CardEffect](#cardeffect)
- [decoder](#decoder)
- [toHtml](#tohtml)

 This Component represents the effect of an individual card.
## Types

### `CardEffect`
```elm
type alias CardEffect  =
    { symbol : Data.CardSymbol.CardSymbol, text : String }
```
 A record containing the text and [Symbol](CardSymbol#CardSymbol) of a card.

---

## Encoders/Decoders

### `decoder`
```elm
decoder : Json.Decode.Decoder CardEffect
```
 Decode a string into a CardEffect.

---

## Views

### `toHtml`
```elm
toHtml : CardEffect -> Html.Html msg
```
 Render the CardEffect as an Html view.
```elm
CardEffect.toHtml card.effect == div [ class "card-effect" ] [ ... ]
```


---


> Generated with elm-make: 0.18.0 and elm-docs: 0.2.2
