module Main exposing (..)
import Html exposing (Html)
import ListExtensions exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String exposing (fromInt, fromFloat)
import List
import Browser
import FontAwesome.Solid
import FontAwesome.Svg exposing (viewIcon)
import Svg.Events exposing (onClick)
import Browser.Events
import Html.Attributes
import SerializableData exposing (..)
import Json.Decode as D

-- diagram

{-

MMMMMMMMMMMMMMMMMMMMMMMMMM -- margin-space at the top
M.---------------.       M -- margin space on left and right, too
M|MMMMMMMMMMMMMMM|       M -- within train-box, another margin-space all around.
M|M.----.I.----.M|I.----.M
M|M|....|I|....|M|I|....|M
M|M'----'I'----'M|I'----'M
M|MMMMMMMMMMMMMMM|       M
M'---------------'       M -- between carriages in box, inter-carriage space
MMMMMMMMMMMMMMMMMMMMMMMMMM -- margin-space on the bottom

Between last carriage (OUTSIDE train-box) and train-box, margin-space + inter-carriage space.

-}

-- Message

type Message
    = InsertCarriage InsertionLocation
--    | AddDescription ChainPosition
    | DeleteCarriage ChainPosition
    | FlipCarriage ChainPosition
    | IncreaseIntensity ChainPosition
    | DecreaseIntensity ChainPosition
    | ShowIntensityHandles ChainPosition
    | ShowControlsFor ChainPosition
    | RemoveUX

init : Diagram
init =
    { config =
        { boxSize = 70 
        , interCarriageSpace = 20
        , marginSize = 10
        }
    , segments =
        [ Concrete NoPreviousCarriage
        ]
    , ux = Nothing
    }

ensureDefaultLinkage : Carriage -> Carriage
ensureDefaultLinkage carriage =
    case carriage of
        Theoretical NoPreviousCarriage ->
            Theoretical (Intensity Exhibiting)
        Concrete NoPreviousCarriage ->
            Concrete (Intensity Exhibiting)
        v ->
            v

modifyTrain : ChainPosition -> (Carriage -> Carriage) -> List Carriage -> List Carriage
modifyTrain position f train =
    List.indexedMap (\i v ->
        if i == position then
            f v
        else
            v
    ) train

insertCarriage : Diagram -> InsertionLocation -> Diagram
insertCarriage diagram location =
    case location of
        After position ->
            { diagram
            | segments =
                List.take (position + 1) diagram.segments
                    ++ (Concrete (Intensity Exhibiting) :: List.drop (position + 1) diagram.segments)
            }
        Before 0 ->
            { diagram
            | segments =
                modifyTrain 0 ensureDefaultLinkage diagram.segments
                |> (\l -> Concrete NoPreviousCarriage :: l)
            }
        Before position ->
            { diagram
            | segments =
                List.take position diagram.segments
                    ++ (Concrete (Intensity Exhibiting) :: List.drop position diagram.segments)
            }

flipCarriage : Diagram -> ChainPosition -> Diagram
flipCarriage diagram position =
    { diagram
    | segments =
        modifyTrain position
            (\carriage ->
                case carriage of
                    Theoretical x ->
                        Concrete x
                    Concrete x ->
                        Theoretical x
            ) diagram.segments
    }

deleteCarriage : Diagram -> ChainPosition -> Diagram
deleteCarriage diagram position =
    { diagram
    | segments =
        List.take position diagram.segments
            ++ List.drop (position + 1) diagram.segments
    }

nextIntensity : Intensity -> Intensity
nextIntensity intensity =
    case intensity of
        Exhibiting ->
            Elaborating
        Elaborating ->
            Linking
        Linking ->
            Creating
        Creating ->
            Creating

prevIntensity : Intensity -> Intensity
prevIntensity intensity =
    case intensity of
        Creating ->
            Linking
        Linking ->
            Elaborating
        Elaborating ->
            Exhibiting
        Exhibiting ->
            Exhibiting

changeIntensity : (Intensity -> Intensity) -> Carriage -> Carriage
changeIntensity f carriage =
    case carriage of
        Theoretical (Intensity x) ->
            Theoretical (Intensity <| f x)
        Concrete (Intensity x) ->
            Concrete (Intensity <| f x)
        v ->
            v

increaseIntensity : Diagram -> ChainPosition -> Diagram
increaseIntensity diagram position =
    { diagram
    | segments =
        modifyTrain position
            (changeIntensity nextIntensity)
            diagram.segments
    }

decreaseIntensity : Diagram -> ChainPosition -> Diagram
decreaseIntensity diagram position =
    { diagram
    | segments =
        modifyTrain position
            (changeIntensity prevIntensity)
            diagram.segments
    }

showInteractable : Diagram -> Interactable -> Diagram
showInteractable diagram interactable =
    { diagram
    | ux = Just interactable
    }

update : Message -> Diagram -> (Diagram, Cmd Message)
update message diagram =
    case message of
        InsertCarriage location ->
            ( insertCarriage diagram location, Cmd.none )
        DeleteCarriage position ->
            ( deleteCarriage diagram position, Cmd.none )
        FlipCarriage position ->
            ( flipCarriage diagram position, Cmd.none )
        IncreaseIntensity position ->
            ( increaseIntensity diagram position, Cmd.none )
        DecreaseIntensity position ->
            ( decreaseIntensity diagram position, Cmd.none )
        ShowIntensityHandles position ->
            ( showInteractable diagram (IntensityHandles position), Cmd.none )
        ShowControlsFor position ->
            ( showInteractable diagram (Controls position), Cmd.none )
        RemoveUX ->
            ( { diagram | ux = Nothing }, Cmd.none )
--            , g
--                [ transform ("translate (5 5) scale (0.05)")
--                ]
--                [ viewIcon FontAwesome.Solid.cloudDownloadAlt ]

dragsTrain : Diagram -> Bool
dragsTrain diagram =
    List.length diagram.segments > 2

widthOfCarriages : Diagram -> List Carriage -> Int
widthOfCarriages diagram segments =
    (List.length segments) * diagram.config.boxSize
        + (List.length segments - 1) * diagram.config.interCarriageSpace

heightOfCarriages : Diagram -> Int
heightOfCarriages diagram =
    diagram.config.boxSize

widthOfDiagram : Diagram -> Int
widthOfDiagram diagram =
     widthOfCarriages diagram diagram.segments
        + 50 -- for any interactables on the right
        + diagram.config.marginSize -- left margin
        + diagram.config.marginSize -- right margin
        + (if dragsTrain diagram then 2 * diagram.config.marginSize else 0)

heightOfDiagram : Diagram -> Int
heightOfDiagram diagram =
    heightOfCarriages diagram
        + 50 -- any interactables on the bottom
        + diagram.config.marginSize -- top margin
        + diagram.config.marginSize -- bottom margin
        + (if dragsTrain diagram then 2 * diagram.config.marginSize else 0)

carriageFill : Carriage -> String
carriageFill carriage =
    case carriage of
        Theoretical _ ->
            "#808080"
        Concrete _ ->
            "white"

connectionOf : Carriage -> DragConnection
connectionOf carriage =
    case carriage of
        Theoretical x ->
            x
        Concrete x ->
            x

carriageTop : Diagram -> Float
carriageTop diagram =
    -- top-margin + train-box margin.
    -- we ALWAYS use both to avoid suddenly shifting Y when a train-box
    -- is created.
    toFloat diagram.config.marginSize * 2.0

carriageCenterY : Diagram -> Float
carriageCenterY diagram =
    carriageTop diagram + toFloat diagram.config.boxSize / 2.0

carriageLeft : Diagram -> ChainPosition -> Float
carriageLeft diagram position =
    toFloat <|
        if dragsTrain diagram then
            if position == List.length diagram.segments - 1 then
                -- we are the last one, and there's a dragged train
                diagram.config.marginSize
                    + widthOfCarriages diagram (List.take position diagram.segments)
                    + diagram.config.interCarriageSpace
                    + diagram.config.marginSize * 2
            else
                diagram.config.marginSize
                    + widthOfCarriages diagram (List.take position diagram.segments)
                    + diagram.config.interCarriageSpace
                    + diagram.config.marginSize
        else
            diagram.config.marginSize
                + widthOfCarriages diagram (List.take position diagram.segments)
                + diagram.config.interCarriageSpace

previousPosition : ChainPosition -> Maybe ChainPosition
previousPosition position =
    if position < 0 then
        Nothing
    else
        Just (position - 1)

previousCarriageLineTerminationX : Diagram -> ChainPosition -> Maybe Float
previousCarriageLineTerminationX diagram position =
    if position <= 0 then
            Nothing
    else
        if dragsTrain diagram && position == List.length diagram.segments - 1 then
            previousPosition position
            |> Maybe.map (\previous ->
                carriageLeft diagram previous
                    + toFloat diagram.config.boxSize
                    + toFloat diagram.config.marginSize -- skip past intra-train margin
            )
        else
            previousPosition position
            |> Maybe.map (\previous ->
                carriageLeft diagram previous
                    + toFloat diagram.config.boxSize
            )

drawHorizontalLine : Float -> Float -> Float -> Svg a
drawHorizontalLine xLeft xRight y =
    line
        [ x1 (fromFloat xLeft)
        , x2 (fromFloat xRight)
        , y1 (fromFloat y)
        , y2 (fromFloat y)
        , stroke "black"
        ]
        []

intensityToString : Intensity -> String
intensityToString intensity =
    case intensity of
        Exhibiting ->
            "Exhibiting"
        Elaborating ->
            "Elaborating"
        Linking ->
            "Linking"
        Creating ->
            "Creating"

drawIntensityInteractables : ChainPosition -> Intensity -> Float -> Float -> Svg Message
drawIntensityInteractables position intensity xCenter yCenter =
    g
        []
        [ if nextIntensity intensity /= intensity then
            g -- 'increase' caret
                [ onClick (IncreaseIntensity position)
                , Svg.Attributes.cursor "pointer"
                ]
                [ circle
                    [ cx (fromFloat xCenter)
                    , cy (fromFloat (yCenter - 32.3))
                    , r "7"
                    , fill "orange"
                    ]
                    []
                , g
                    [ transform ("translate ("
                        ++ fromFloat (xCenter - 7.6) ++ " "
                        ++ fromFloat (yCenter - 40) ++ ") scale (0.03)")
                    ]
                    [ viewIcon FontAwesome.Solid.chevronCircleUp ]
                , Svg.title [] [text (intensityToString intensity ++ " ➡ " ++ intensityToString (nextIntensity intensity))]
                ]
          else
            g [] []
        , if prevIntensity intensity /= intensity then
            g -- 'decrease' caret
                [ onClick (DecreaseIntensity position)
                , Svg.Attributes.cursor "pointer"
                ]
                [ circle
                    [ cx (fromFloat xCenter)
                    , cy (fromFloat (yCenter + 31.7))
                    , r "7"
                    , fill "orange"
                    ]
                    []
                , g
                    [ transform ("translate ("
                        ++ fromFloat (xCenter - 7.6) ++ " "
                        ++ fromFloat (yCenter + 24) ++ ") scale (0.03)")
                    ]
                    [ viewIcon FontAwesome.Solid.chevronCircleDown ]
                , Svg.title [] [text (intensityToString intensity ++ " ➡ " ++ intensityToString (prevIntensity intensity))]
                ]
          else
            g [] []
        ]

drawIntensity : Diagram -> ChainPosition -> Float -> Float -> Intensity -> Svg Message
drawIntensity diagram position xLeft xRight intensity =
    let
        interactables =
            if diagram.ux == Just (IntensityHandles position) then
                drawIntensityInteractables position intensity ((xLeft + xRight) / 2) (carriageCenterY diagram)
            else
                g [] []
    in
    case intensity of
        Exhibiting ->
            g
                []
                [ drawHorizontalLine xLeft xRight (carriageCenterY diagram)
                , interactables
                ]
        Elaborating ->
            g
                []
                [ drawHorizontalLine xLeft xRight (carriageCenterY diagram - 4)
                , drawHorizontalLine xLeft xRight (carriageCenterY diagram + 4)                
                ,  interactables
                ]
        Linking ->
            g
                []
                [ drawHorizontalLine xLeft xRight (carriageCenterY diagram)
                , drawHorizontalLine xLeft xRight (carriageCenterY diagram - 8)
                , drawHorizontalLine xLeft xRight (carriageCenterY diagram + 8)
                , interactables
                ]
        Creating ->
            g
                []
                [ drawHorizontalLine xLeft xRight (carriageCenterY diagram - 4)
                , drawHorizontalLine xLeft xRight (carriageCenterY diagram - 12)
                , drawHorizontalLine xLeft xRight (carriageCenterY diagram + 4)
                , drawHorizontalLine xLeft xRight (carriageCenterY diagram + 12)
                , interactables
                ]

drawConnection : Diagram -> ChainPosition -> Carriage -> Svg Message
drawConnection diagram position carriage =
    case connectionOf carriage of
        Intensity intensity ->
            previousCarriageLineTerminationX diagram position
            |> Maybe.map (\xTermination ->
                drawIntensity diagram position xTermination (carriageLeft diagram position) intensity
            )
            |> Maybe.withDefault (g [] [])
        NoPreviousCarriage ->
            g [] []

drawCarriage : Diagram -> ChainPosition -> Carriage -> Svg Message
drawCarriage diagram position carriage =
    g
        []
        [ rect
            [ x (fromFloat (carriageLeft diagram position))
            , y (fromFloat (carriageTop diagram))
            , width (fromInt diagram.config.boxSize)
            , height (fromInt diagram.config.boxSize)
            , fill (carriageFill carriage)
            --, onClick (FlipCarriage position)
            , stroke "black"
            ]
            []
        , drawConnection diagram position carriage
        , if diagram.ux == Just (Controls position) then
            g
                []
                [ g
                    [ onClick (InsertCarriage (Before position))
                    , Svg.Attributes.cursor "pointer"
                    ]
                    [ circle
                        [ cx (fromFloat (carriageLeft diagram position + 9.7))
                        , cy (fromFloat (carriageCenterY diagram - 0.3))
                        , r "7.4"
                        , fill "white"
                        ]
                        []
                    , g
                        [ transform ("translate ("
                            ++ fromFloat (carriageLeft diagram position + 2) ++ " "
                            ++ fromFloat (carriageCenterY diagram - 8)
                            ++ ") scale (0.03)")
                        ]
                        [ viewIcon FontAwesome.Solid.plusCircle ]
                    , Svg.title [] [text "Insert carriage on left"]
                    ]
                , g
                    [ onClick (InsertCarriage (After position))
                    , Svg.Attributes.cursor "pointer"
                    ]
                    [ circle
                        [ cx (fromFloat (carriageLeft diagram position + toFloat diagram.config.boxSize - 9.4))
                        , cy (fromFloat (carriageCenterY diagram - 0.3))
                        , r "7.4"
                        , fill "white"
                        ]
                        []
                    , g
                        [ transform ("translate ("
                            ++ fromFloat (carriageLeft diagram position + toFloat diagram.config.boxSize - 17) ++ " "
                            ++ fromFloat (carriageCenterY diagram - 8)
                            ++ ") scale (0.03)") ]
                        [ viewIcon FontAwesome.Solid.plusCircle ]
                    , Svg.title [] [text "Insert carriage on right"]
                    ]
                , if List.length diagram.segments > 1 then
                    g
                        [ onClick (DeleteCarriage position)
                        , Svg.Attributes.cursor "pointer"
                        , color "red"
                        , stroke "black"
                        , strokeWidth "25"
                        ]
                        [ g
                            [ transform ("translate ("
                                ++ fromFloat (carriageLeft diagram position + toFloat diagram.config.boxSize - 14) ++ " "
                                ++ fromFloat (carriageTop diagram + toFloat diagram.config.boxSize - 16)
                                ++ ") scale (0.025)")
                            ]
                            [ viewIcon FontAwesome.Solid.trash
                            ]
                        , Svg.title [] [text "Delete carriage"]
                        ]
                else
                    g [] []
                , g
                    [ onClick (FlipCarriage position)
                    , Svg.Attributes.cursor "pointer"
                    ]
                    [ rect
                        [ x (fromFloat (carriageLeft diagram position + 3))
                        , y (fromFloat (carriageTop diagram + 3))
                        , width "18.5"
                        , height "15"
                        , rx "7"
                        , fill "white"
                        ]
                        []
                    , g
                        [ transform ("translate ("
                            ++ fromFloat (carriageLeft diagram position + 3) ++ " "
                            ++ fromFloat (carriageTop diagram + 3)
                            ++ ") scale (0.03)")
                        ]
                        [ viewIcon FontAwesome.Solid.theaterMasks
                        ]
                    , Svg.title [] [text "Switch between theoretical/concrete"]
                    ]
                ]
          else
            g [] []
        ]

drawTrainBox : Diagram -> Svg a
drawTrainBox diagram =
    if dragsTrain diagram then
        rect
            [ x (fromInt (diagram.config.marginSize))
            , y (fromInt diagram.config.marginSize)
            , width (fromInt (diagram.config.marginSize * 2 + widthOfCarriages diagram (List.take (List.length diagram.segments - 1) diagram.segments) ))
            , height (fromInt (diagram.config.marginSize * 2 + heightOfCarriages diagram))
            , stroke "black"
            , fill "transparent"
            ]
            []
    else
        g [] []

drawCarriages : Diagram -> Svg Message
drawCarriages diagram =
    g
        []
        [ drawTrainBox diagram
        , g
            []
            ( List.indexedMap (drawCarriage diagram) diagram.segments )
        ]

svgView : Diagram -> Svg Message
svgView diagram =
  svg
    [ width (fromInt (widthOfDiagram diagram))
    , height (fromInt (heightOfDiagram diagram))
    , viewBox ("0 0 " ++ fromInt (widthOfDiagram diagram) ++ " " ++ fromInt (heightOfDiagram diagram))
    ]
    [ drawCarriages diagram
    ]

pointWithinCarriage : Diagram -> (Int, Int) -> Maybe ChainPosition
pointWithinCarriage diagram (x, y) =
    first
        (\position ->
            let
                left =
                    round <| carriageLeft diagram position
                top =
                    round <| carriageTop diagram
                right =
                    left + diagram.config.boxSize
                bottom =
                    top + diagram.config.boxSize

            in
                left <= x && right >= x && top <= y && bottom >= y
        )
        (List.range 0 (List.length diagram.segments - 1))

pointNearLink : Diagram -> (Int, Int) -> Maybe ChainPosition
pointNearLink diagram (x, y) =
    first
        (\position ->
            let
                prevRight =
                    (round <| carriageLeft diagram (position - 1)) + diagram.config.boxSize
                nextLeft =
                    (round <| carriageLeft diagram position)
                centerY =
                    round <| carriageCenterY diagram
            in
                prevRight <= x && nextLeft >= x && centerY - 45 <= y && centerY + 45 >= y
        )
        (List.range 1 (List.length diagram.segments - 1))


withinInteractableBoundaries : Diagram -> D.Decoder Message
withinInteractableBoundaries diagram =
    D.map2 (\x y -> (x, y))
        (D.field "clientX" D.int)
        (D.field "clientY" D.int)
    |> D.andThen
        (\coords ->
            case ( pointWithinCarriage diagram coords, pointNearLink diagram coords ) of
                ( Just position, _ ) ->
                    D.succeed (ShowControlsFor position)
                ( _, Just position ) ->
                    D.succeed (ShowIntensityHandles position)
                _ ->
                    case diagram.ux of
                        Nothing ->
                            D.fail "No point within interactable bounds (and that's OK!)"
                        Just _ ->
                            D.succeed RemoveUX
        )

view : Diagram -> Html Message
view diagram =
    Html.div
        [ Svg.Events.on "mousemove" (withinInteractableBoundaries diagram)
        ]
        [ svgView diagram ]

subscriptions : Diagram -> Sub a
subscriptions _ =
    Sub.none

main : Program () Diagram Message
main =
    Browser.element
        { init = \_ -> (init, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }