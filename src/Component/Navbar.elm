module Component.Navbar
    exposing
        ( Navigation
        , RouteDetails
        , defaults
        , toHtml
        , toHtmlLazy
        )

{-| Component.Patrons


# Types

@docs Patrons


# Build

@docs defaults


# Encoders/Decoders


# Views

@docs toHtml, toHtmlLazy

-}

import Html exposing (Html, nav)
import Html.Lazy exposing (lazy2)
import Html.Attributes exposing (class)
import Route exposing (Route)


{-| A list containing names of our patrons.
-}
type alias Navigation =
    List RouteDetails


type alias RouteDetails =
    { route : Route, class : String, icon : String }


{-| A default list of our patrons. This is not dynamic currently.
-}
defaults : Navigation
defaults =
    [ { route = Route.Home, class = "logo", icon = "/icons/logo.png" }
    , { route = Route.Search, class = "navitem-search", icon = "/icons/search.final.svg" }
    , { route = Route.Home, class = "navitem-cards", icon = "/icons/cards.final.svg" }
    , { route = Route.Deck, class = "navitem-deck", icon = "/icons/deck.final.svg" }
    , { route = Route.Info, class = "navitem-info", icon = "/icons/info.final.svg" }

    -- TODO: Download "/icons/ios-download-outline.svg"
    ]


{-| Render the Navigation as an Html view.
-}
toHtml : (RouteDetails -> Html msg) -> Navigation -> Html msg
toHtml renderChild navigation =
    nav [ class "navbar" ] (List.map renderChild navigation)


{-| Render the Patrons as a Lazy Html view to avoid re-rendering if we are using something like defaults.
-}
toHtmlLazy : (RouteDetails -> Html msg) -> Navigation -> Html msg
toHtmlLazy =
    lazy2 toHtml
