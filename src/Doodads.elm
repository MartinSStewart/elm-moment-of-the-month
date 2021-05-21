module Doodads exposing (..)

import Element exposing (Element)
import Element.Background
import Html.Attributes
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)


grass : Quantity Int Pixels -> Element msg
grass yOffset =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 42
        , Element.Background.tiledX "./grass.png"
        , Element.moveDown <| toFloat (Pixels.inPixels yOffset - 3)
        ]
        Element.none


tree : Quantity Int Pixels -> Element msg
tree yOffset =
    Element.el
        [ Element.moveDown <| toFloat (Pixels.inPixels yOffset - 124), Element.moveRight 200 ]
        (Element.image
            [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
            , Element.scale 3
            ]
            { src = "./tree.png", description = "" }
        )


butterflies : Quantity Int Pixels -> Element msg
butterflies yOffset =
    Element.el
        [ Element.moveDown <| toFloat (Pixels.inPixels yOffset - 50), Element.moveLeft 80, Element.alignRight ]
        (Element.image
            [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
            , Element.scale 3
            ]
            { src = "./butterflies.gif", description = "" }
        )


flowers : Quantity Int Pixels -> Element msg
flowers yOffset =
    Element.el
        [ Element.moveDown <| toFloat (Pixels.inPixels yOffset - 20), Element.moveRight 50 ]
        (Element.image
            [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
            , Element.scale 3
            ]
            { src = "./flowers.png", description = "" }
        )


flowers2 : Quantity Int Pixels -> Element msg
flowers2 yOffset =
    Element.el
        [ Element.moveDown <| toFloat (Pixels.inPixels yOffset - 20), Element.moveLeft 80, Element.alignRight ]
        (Element.image
            [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
            , Element.scale 3
            ]
            { src = "./flowers.png", description = "" }
        )


mountain : Quantity Int Pixels -> Element msg
mountain yOffset =
    Element.el
        [ Element.moveDown <| toFloat (Pixels.inPixels yOffset - 100), Element.alignRight, Element.alpha 0.5 ]
        (Element.image
            [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
            , Element.scale 3
            ]
            { src = "./mountains.png", description = "" }
        )


cloud1 : Quantity Int Pixels -> Element msg
cloud1 yOffset =
    Element.el
        [ Element.moveDown <| toFloat (Pixels.inPixels yOffset - 600), Element.moveRight 200 ]
        (Element.el
            [ Element.htmlAttribute <| Html.Attributes.style "animation-name" "cloud-drift"
            , Element.htmlAttribute <| Html.Attributes.style "animation-timing-function" "linear"
            , Element.htmlAttribute <| Html.Attributes.style "animation-duration" "1000s"
            , Element.htmlAttribute <| Html.Attributes.style "animation-iteration-count" "infinite"
            ]
            (Element.image
                [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
                , Element.scale 3
                ]
                { src = "./cloud.png", description = "" }
            )
        )


cloud2 : Quantity Int Pixels -> Element msg
cloud2 yOffset =
    Element.el
        [ Element.moveDown <| toFloat (Pixels.inPixels yOffset - 800), Element.moveRight 800 ]
        (Element.el
            [ Element.htmlAttribute <| Html.Attributes.style "animation-name" "cloud-drift"
            , Element.htmlAttribute <| Html.Attributes.style "animation-timing-function" "linear"
            , Element.htmlAttribute <| Html.Attributes.style "animation-duration" "500s"
            , Element.htmlAttribute <| Html.Attributes.style "animation-iteration-count" "infinite"
            ]
            (Element.image
                [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
                , Element.scale 3
                ]
                { src = "./cloud.png", description = "" }
            )
        )


cloud3 : Quantity Int Pixels -> Element msg
cloud3 yOffset =
    Element.el
        [ Element.moveDown <| toFloat (Pixels.inPixels yOffset - 1000)
        , Element.moveRight 1200
        ]
        (Element.el
            [ Element.htmlAttribute <| Html.Attributes.style "animation-name" "cloud-drift"
            , Element.htmlAttribute <| Html.Attributes.style "animation-timing-function" "linear"
            , Element.htmlAttribute <| Html.Attributes.style "animation-duration" "800s"
            , Element.htmlAttribute <| Html.Attributes.style "animation-iteration-count" "infinite"
            ]
            (Element.image
                [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
                , Element.scale 3
                ]
                { src = "./cloud.png", description = "" }
            )
        )


treasure : Quantity Int Pixels -> Element msg
treasure yOffset =
    Element.el
        [ Element.moveDown <| toFloat (Pixels.inPixels yOffset + 150)
        , Element.moveLeft 400
        , Element.alignRight
        ]
        (Element.el
            []
            (Element.image
                [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
                , Element.scale 3
                ]
                { src = "./treasure.png", description = "" }
            )
        )


skeleton : Quantity Int Pixels -> Element msg
skeleton yOffset =
    Element.el
        [ Element.moveDown <| toFloat (Pixels.inPixels yOffset + 180)
        , Element.moveRight 200
        ]
        (Element.el
            []
            (Element.image
                [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
                , Element.scale 3
                ]
                { src = "./skeleton.png", description = "" }
            )
        )


sun : Element msg
sun =
    Element.image
        [ Element.htmlAttribute <| Html.Attributes.class "pixel-art"
        , Element.scale 3
        , Element.alignRight
        , Element.moveDown 32
        , Element.moveLeft 32
        ]
        { src = "./sun.gif", description = "" }
