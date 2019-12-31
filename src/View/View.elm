module View.View exposing (view)

import Array exposing (..)
import Debug
import FontAwesome exposing (icon, search)
import Html exposing (Html, a, br, button, div, h1, h2, h3, h4, h5, i, img, input, label, li, option, p, pre, select, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, href, id, placeholder, property, rows, scope, selected, src, style, type_, value)
import Html.Attributes.Aria exposing (ariaExpanded, ariaHasPopup, ariaHidden, ariaLabel, ariaLabelledby, role)
import Html.Events exposing (onClick, onInput, onSubmit)
import Iso8601
import Model.Model exposing (..)
import Round
import Time exposing (Posix, millisToPosix, toHour, toMinute, utc)


viewElement : Model -> Html Msg
viewElement model =
    case model of
        Failure ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text "Something went wrong." ]
                    ]
                ]

        NotImplementedYet ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text "This function has not been implemented yet." ]
                    ]
                ]

        HomePage ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text "Velkommen til lov søgemaskinen." ]
                    ]
                ]

        LoadingElements ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text "Loading All Tasks. Please be patient the first time. " ]
                    ]
                ]

        DisplayingElements textFilter elements ->
            let
                filterExist =
                    textFilter /= ""

                fileredElements =
                    List.filter
                        (\element -> String.contains (String.toLower textFilter) (String.toLower (element.content ++ element.paragraphText)))
                        elements
            in
            div [ class "card", style "width" "22rem" ]
                [ div []
                    [ div [ class "input-group p-2" ]
                        [ div [ class "input-group-prepend" ]
                            [ span [ class "input-group-text" ] [ text "Filter" ]
                            ]
                        , input
                            [ type_ "text"
                            , placeholder "Search for"
                            , onInput (FilterOnTextElements elements)
                            , value textFilter
                            , class "form-control"
                            ]
                            []
                        ]
                    ]
                , table
                    [ class "card-table table table-hover" ]
                    [ thead [ class "card-header" ]
                        [ tr []
                            [ if filterExist then
                                th [ scope "col" ] [ text "Filteret tekst" ]

                              else
                                th [ scope "col" ]
                                    [ text "Den fulde tekst" ]
                            ]
                        ]
                    , tbody [] (List.concat (List.map (\element -> viewElementLine textFilter element) fileredElements))
                    ]
                ]


viewElementLine : String -> Element -> List (Html Msg)
viewElementLine textFilter element =
    let
        filterExist =
            textFilter /= ""

        rows =
            case element.context of
                "Lov" ->
                    [ tr []
                        [ th
                            [ scope "row" ]
                            [ h5 [] [ text element.chapterText ] ]
                        ]
                    ]

                "Kapitel" ->
                    [ tr []
                        [ td
                            []
                            [ span [ class "font-weight-bold" ] [ text (element.chapterId ++ ": ") ]
                            , span [] [ text element.chapterText ]
                            ]
                        ]
                    ]

                "Paragraf" ->
                    [ tr []
                        [ td
                            []
                            [ span [ class "font-weight-bold" ] [ text (element.paragraphText ++ " ") ]
                            , span [] [ text element.content ]
                            ]
                        ]
                    ]

                "Styk" ->
                    [ tr []
                        [ td
                            []
                            [ span [ class "font-weight-bold" ] [ text (element.paragraphText ++ " ") ]
                            , span [] [ text element.content ]
                            ]
                        ]
                    ]

                "Underskrift" ->
                    [ tr [] [ th [] [ text element.context ] ]
                    , tr []
                        [ td
                            []
                            [ span [ class "font-weight-bold" ] [ text (element.chapterText ++ " ") ]
                            ]
                        ]
                    , tr []
                        [ td
                            []
                            [ span [ class "font-weight-bold" ] [ text (element.paragraphText ++ " ") ]
                            ]
                        ]
                    , tr []
                        [ td
                            []
                            [ span [] [ text element.content ]
                            ]
                        ]
                    ]

                _ ->
                    [ tr [] [ td [] [ text element.content ] ] ]
    in
    rows


showElementField : Element -> ElementField -> Bool
showElementField element elementField =
    case elementField of
        _ ->
            True


viewElementField : Element -> ElementField -> Html Msg
viewElementField taskEntity taskField =
    let
        displayField =
            showElementField taskEntity taskField
    in
    if displayField then
        case taskField of
            Content ->
                pre [ class "card-title text-primary" ]
                    [ text (String.concat [ taskEntity.content ]) ]

    else
        span [] []


iso8601ToHoursMinutes : String -> String
iso8601ToHoursMinutes jsonDateTime =
    String.slice 11 16 jsonDateTime


iso8601ToDateTime : String -> String
iso8601ToDateTime jsonDateTime =
    String.slice 0 10 jsonDateTime


iso8601ToWeekday : String -> String
iso8601ToWeekday jsonDateTime =
    case Iso8601.toTime jsonDateTime of
        Ok posix ->
            ": " ++ toEnglishWeekday (Time.toWeekday Time.utc posix)

        Err _ ->
            ""


toEnglishWeekday : Time.Weekday -> String
toEnglishWeekday weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"



-- 2012-04-23T18:25:43.511Z
-- 012345678901234567890123
--           1         2


timeToString : Int -> Int -> String
timeToString time timezone =
    let
        milliseconds =
            (time + timezone) * 1000

        posix =
            Time.millisToPosix milliseconds

        hours =
            Time.toHour utc posix

        minutes =
            Time.toMinute utc posix

        seconds =
            Time.toSecond utc posix

        hoursString =
            String.fromInt hours

        minutesString =
            (if minutes < 10 then
                "0"

             else
                ""
            )
                ++ String.fromInt minutes

        secondsString =
            (if seconds < 10 then
                "0"

             else
                ""
            )
                ++ String.fromInt seconds
    in
    String.concat [ hoursString, ":", minutesString, ":", secondsString ]


viewElementMenuButton : Model -> Html Msg
viewElementMenuButton model =
    div [ class "btn-group", role "group" ]
        [ button
            [ class "btn btn-outline-primary dropdown-toggle"
            , type_ "button"
            , id "dropdownTaskMenuButton"
            , attribute "data-toggle" "dropdown"
            , ariaHasPopup "menu"
            , ariaExpanded "false"
            ]
            [ text "Lov" ]
        , div
            [ class "dropdown-menu"
            , ariaLabelledby "dropdownMenuButton"
            ]
            [ a
                [ class "dropdown-item"
                , href "#"
                , onClick GetElements
                ]
                [ text "Hent lejeloven" ]
            ]
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "p-2" ]
        [ div [ class "btn-toolbar", role "toolbar", ariaLabel "Toolbar" ]
            [ div [ class "btn-group mr-2", role "group", ariaLabel "Element Menu Buttons" ]
                [ viewElementMenuButton model
                ]
            , div [ class "btn-group mr-2", role "group", ariaLabel "Element Menu Buttons" ]
                [ button
                    [ type_ "button"
                    , class "btn btn-outline-primary"
                    , href "#"
                    , onClick GoToHomePage
                    ]
                    [ text "Home" ]
                ]
            ]
        ]


solidBlackBorder : Html.Attribute Msg
solidBlackBorder =
    style "border" "1px solid black"


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ div [ class "col" ] [ viewMenu model ] ]
        , div [ class "row" ]
            [ div [ class "col" ] [ viewElement model ] ]
        ]
