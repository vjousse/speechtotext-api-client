module UI exposing (displayNotification, guestLayout, layout)

import Domain.Config exposing (ServerUrl)
import Domain.Notification as DN exposing (Notification)
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Shared exposing (User)
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr


viewLink : String -> Route -> Html msg
viewLink label route =
    a [ href (Route.toHref route) ] [ text label ]


guestLayout : List (Html msg) -> List (Html msg)
guestLayout children =
    [ div [ class "min-h-screen bg-gray-50 flex flex-col justify-center py-12 sm:px-6 lg:px-8" ] children ]


layout : User -> ServerUrl -> List (Html msg) -> List (Html msg)
layout user serverUrl children =
    [ div [ class "h-screen flex overflow-hidden bg-gray-100" ]
        [ sidebar user serverUrl
        , content children

        --, header [ class "navbar" ]
        --    [ strong [ class "brand" ] [ viewLink "Home" Route.Home_ ]
        --    , viewLink "Static" Route.Static
        --    , div [ class "splitter" ] []
        --    , viewLink "Dynamic: Apple" (Route.Dynamic__Name_ { name = "apple" })
        --    , viewLink "Dynamic: Banana" (Route.Dynamic__Name_ { name = "banana" })
        --    ]
        ]
    ]


content : List (Html msg) -> Html msg
content children =
    div [ class "flex flex-col w-0 flex-1 overflow-hidden" ]
        [ div [ class "md:hidden pl-1 pt-1 sm:pl-3 sm:pt-3" ]
            [ button [ class "-ml-0.5 -mt-0.5 h-12 w-12 inline-flex items-center justify-center rounded-md text-gray-500 hover:text-gray-900 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500" ]
                [ span [ class "sr-only" ]
                    [ text "Open sidebar" ]
                , svg [ attribute "aria-hidden" "true", SvgAttr.class "h-6 w-6", SvgAttr.fill "none", attribute "stroke" "currentColor", SvgAttr.viewBox "0 0 24 24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                    [ path [ SvgAttr.d "M4 6h16M4 12h16M4 18h16", attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "stroke-width" "2" ]
                        []
                    ]
                ]
            ]
        , main_ [ class "flex-1 relative z-0 overflow-y-auto focus:outline-none" ]
            children
        ]


menuLink : String -> Route -> Bool -> Svg.Svg msg -> Html msg
menuLink label route active icon =
    let
        linkClass =
            if active then
                "bg-gray-900 text-white "

            else
                "text-gray-300 hover:bg-gray-700 hover:text-white "

        svgClass =
            if active then
                "text-gray-300 "

            else
                "text-gray-400 group-hover:text-gray-300 "
    in
    a [ class <| linkClass ++ "group flex items-center px-2 py-2 text-sm font-medium rounded-md", href (Route.toHref route) ]
        [ svg [ attribute "aria-hidden" "true", SvgAttr.class <| svgClass ++ "mr-3 flex-shrink-0 h-6 w-6", SvgAttr.fill "none", attribute "stroke" "currentColor", SvgAttr.viewBox "0 0 24 24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
            [ icon
            ]
        , text label
        ]


sidebar : User -> ServerUrl -> Html msg
sidebar user serverUrl =
    div [ class "hidden md:flex md:flex-shrink-0" ]
        [ div [ class "flex flex-col w-64" ]
            [ div [ class "flex flex-col h-0 flex-1 bg-gray-800" ]
                [ div [ class "flex-1 flex flex-col pt-5 pb-4 overflow-y-auto" ]
                    [ div [ class "flex items-center flex-shrink-0 px-4" ]
                        [ img [ alt "Workflow", class "h-8 w-auto", src "https://tailwindui.com/img/logos/workflow-logo-indigo-500-mark-white-text.svg" ]
                            []
                        ]
                    , nav [ class "mt-5 flex-1 px-2 bg-gray-800 space-y-1" ]
                        [ menuLink "Files"
                            Route.Home_
                            True
                            (path [ SvgAttr.d "M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6", attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "stroke-width" "2" ]
                                []
                            )
                        , menuLink "Logout"
                            Route.SignOut
                            False
                            (path [ SvgAttr.d "M17 16l4-4m0 0l-4-4m4 4H7m6 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h4a3 3 0 013 3v1", attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "stroke-width" "2" ]
                                []
                            )
                        ]
                    ]
                , div [ class "flex-shrink-0 flex bg-gray-700 p-4" ]
                    [ a [ class "flex-shrink-0 w-full group block", href "#" ]
                        [ div [ class "flex items-center" ]
                            [ div []
                                [ img [ alt "", class "inline-block h-9 w-9 rounded-full", src (serverUrl ++ "/assets/no-avatar.png") ]
                                    []
                                ]
                            , div [ class "ml-3" ]
                                [ p [ class "text-sm font-medium text-white" ]
                                    [ text user.email ]
                                , p [ class "text-xs font-medium text-gray-300 group-hover:text-gray-200" ]
                                    [ text "" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


displayNotification : Maybe Notification -> msg -> Html msg
displayNotification maybeNotification clearMessage =
    case maybeNotification of
        Just notification ->
            div
                [ attribute "aria-live" "assertive"
                , class "fixed inset-0 flex items-end px-4 py-6 pointer-events-none sm:p-6 sm:items-start"
                ]
                [ div
                    [ class "w-full flex flex-col items-center space-y-4 sm:items-end"
                    ]
                    [ div
                        [ class "max-w-sm w-full bg-white shadow-lg rounded-lg pointer-events-auto ring-1 ring-black ring-opacity-5 overflow-hidden"
                        ]
                        [ div
                            [ class "p-4"
                            ]
                            [ div
                                [ class "flex items-start"
                                ]
                                [ div
                                    [ class "flex-shrink-0"
                                    ]
                                    [ {- Heroicon name: outline/check-circle -}
                                      case notification.type_ of
                                        DN.Success ->
                                            svg
                                                [ SvgAttr.class "h-6 w-6 text-green-400"
                                                , SvgAttr.fill "none"
                                                , SvgAttr.viewBox "0 0 24 24"
                                                , SvgAttr.stroke "currentColor"
                                                , attribute "aria-hidden" "true"
                                                ]
                                                [ path
                                                    [ SvgAttr.strokeLinecap "round"
                                                    , SvgAttr.strokeLinejoin "round"
                                                    , SvgAttr.strokeWidth "2"
                                                    , SvgAttr.d "M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
                                                    ]
                                                    []
                                                ]

                                        DN.Error ->
                                            svg
                                                [ SvgAttr.class "h-6 w-6 text-red-400"
                                                , SvgAttr.fill "none"
                                                , SvgAttr.viewBox "0 0 24 24"
                                                , SvgAttr.stroke "currentColor"
                                                , attribute "aria-hidden" "true"
                                                ]
                                                [ path
                                                    [ SvgAttr.strokeLinecap "round"
                                                    , SvgAttr.strokeLinejoin "round"
                                                    , SvgAttr.strokeWidth "2"
                                                    , SvgAttr.d "M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
                                                    ]
                                                    []
                                                ]
                                    ]
                                , div
                                    [ class "ml-3 w-0 flex-1 pt-0.5"
                                    ]
                                    [ p
                                        [ class "text-sm font-medium text-gray-900"
                                        ]
                                        [ text notification.title ]
                                    , case notification.subtitle of
                                        Just subtitle ->
                                            p
                                                [ class "mt-1 text-sm text-gray-500"
                                                ]
                                                [ text subtitle ]

                                        Nothing ->
                                            text ""
                                    ]
                                , div
                                    [ class "ml-4 flex-shrink-0 flex"
                                    ]
                                    [ button
                                        [ class "bg-white rounded-md inline-flex text-gray-400 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                                        , onClick clearMessage
                                        ]
                                        [ span
                                            [ class "sr-only"
                                            ]
                                            [ text "Close" ]
                                        , {- Heroicon name: solid/x -}
                                          svg
                                            [ SvgAttr.class "h-5 w-5"
                                            , SvgAttr.viewBox "0 0 20 20"
                                            , SvgAttr.fill "currentColor"
                                            , attribute "aria-hidden" "true"
                                            ]
                                            [ path
                                                [ SvgAttr.fillRule "evenodd"
                                                , SvgAttr.d "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                                                , SvgAttr.clipRule "evenodd"
                                                ]
                                                []
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

        _ ->
            text ""
