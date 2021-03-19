module Main exposing (main)

import Browser
import Color.OneDark as OneDark exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Random
import Random.List
import Tuple3


type alias Model =
    { players : List Player
    , teams : List ( Player, Player )
    }


type alias Player =
    { color : Element.Color
    , name : String
    , showed : Bool
    , mission : String
    }


view model =
    Element.layout
        [ Element.width Element.fill, Element.centerX ]
        (Element.column
            [ Background.color (Element.rgba255 255 255 255 1)
            , Background.image
                "https://cdn.pixabay.com/photo/2019/06/30/20/55/board-of-risk-4308773_1280.png"
            , Font.color (Element.rgba255 46 52 54 1)
            , Font.family
                [ Font.typeface "system-ui"
                , Font.typeface "-apple-system"
                , Font.typeface "sans-serif"
                ]
            , Font.size 16
            , Element.height Element.fill
            , Element.width Element.fill
            , spacing 32
            ]
            [ Element.paragraph
                [ Font.center
                , Font.letterSpacing 5
                , Font.bold
                , Font.color (Element.rgba255 46 52 54 1)
                , Font.size 36
                , Element.height Element.shrink
                , Element.width Element.fill
                , Element.paddingXY 16 16
                , Region.heading 1
                ]
                [ Element.text "Teams-Risk" ]
            , Input.button
                [ Background.color (Element.rgba255 52 101 164 1)
                , Element.centerY
                , Element.centerX
                , Font.center
                , Font.color (Element.rgba255 255 255 255 1)
                , Font.size 30
                , Element.height Element.shrink
                , Element.width Element.shrink
                , Element.paddingXY 16 16
                , Border.rounded 2
                , Border.color (Element.rgba255 52 101 164 1)
                , Border.solid
                , Border.widthXY 1 1
                ]
                { onPress = Just BeginGame
                , label = Element.text "\u{1F977} BEGIN THE GAME 🧙\u{200D}♂️"
                }
            , el [ centerX, centerY ] <| showTeams model.teams
            ]
        )


showTeams : List ( Player, Player ) -> Element Msg
showTeams teams =
    row
        [ centerX
        , width fill
        , height fill
        , spacing 32
        ]
        (List.map showTeam teams)


showTeam : ( Player, Player ) -> Element Msg
showTeam team =
    column
        [ spacing 32
        ]
        [ showPlayer <| Tuple.first team, showMissionInputIfClicked <| Tuple.second team, showPlayerIfClicked <| Tuple.second team ]


showMissionInputIfClicked player =
    if player.showed then
        column [ width <| px 150, height <| px 100, Background.color player.color ]
            [ el [ centerX, centerY ] <|
                Input.text []
                    { onChange = ChangeMission player.name
                    , text = player.mission
                    , placeholder = Nothing
                    , label = Input.labelHidden "Mission"
                    }
            ]

    else
        none


showPlayerIfClicked player =
    if player.showed then
        column [ width <| px 150, height <| px 150, Background.color player.color ]
            [ el [ centerX, centerY, Font.size 22 ] <| text player.name ]

    else
        none


showPlayer player =
    column [ Events.onClick <| ShowTeamMate player.name, pointer, width <| px 150, height <| px 150, Background.color player.color, Border.rounded 9 ]
        [ el [ centerX, centerY, Font.size 22 ] <| text player.name ]


type Msg
    = CheckboxClicked Bool
    | RadioClicked Int
    | TextChanged String
    | BeginGame
    | ReceiveRandomList (List Player)
    | ShowTeamMate String
    | ChangeMission String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckboxClicked value ->
            ( model, Cmd.none )

        RadioClicked value ->
            ( model, Cmd.none )

        TextChanged value ->
            ( model, Cmd.none )

        BeginGame ->
            ( model, Random.generate ReceiveRandomList (Random.List.shuffle model.players) )

        ReceiveRandomList newList ->
            let
                teams =
                    case newList of
                        [ a, b, c, d, e, f ] ->
                            [ ( a, b ), ( c, d ), ( e, f ), ( b, a ), ( d, c ), ( f, e ) ]

                        _ ->
                            []
            in
            ( { model | players = newList, teams = teams }, Cmd.none )

        ShowTeamMate playerName ->
            ( { model | teams = updateTeamsWithShowed playerName model.teams }, Cmd.none )

        ChangeMission playerName newMissionInput ->
            ( { model | teams = updateTeamsWithNewMission playerName newMissionInput model.teams }, Cmd.none )


updateTeamsWithShowed playerName teams =
    let
        newTeams =
            case teams of
                [ ( a, aa ), ( b, bb ), ( c, cc ), ( d, dd ), ( e, ee ), ( f, ff ) ] ->
                    if a.name == playerName then
                        [ ( a
                          , { aa | showed = not aa.showed }
                          )
                        , ( b, bb )
                        , ( c, cc )
                        , ( d, dd )
                        , ( e, ee )
                        , ( f, ff )
                        ]

                    else if b.name == playerName then
                        [ ( a, aa )
                        , ( b, { bb | showed = not bb.showed } )
                        , ( c, cc )
                        , ( d, dd )
                        , ( e, ee )
                        , ( f, ff )
                        ]

                    else if c.name == playerName then
                        [ ( a, aa )
                        , ( b, bb )
                        , ( c, { cc | showed = not cc.showed } )
                        , ( d, dd )
                        , ( e, ee )
                        , ( f, ff )
                        ]

                    else if d.name == playerName then
                        [ ( a, aa )
                        , ( b, bb )
                        , ( c, cc )
                        , ( d, { dd | showed = not dd.showed } )
                        , ( e, ee )
                        , ( f, ff )
                        ]

                    else if e.name == playerName then
                        [ ( a, aa )
                        , ( b, bb )
                        , ( c, cc )
                        , ( d, dd )
                        , ( e, { ee | showed = not ee.showed } )
                        , ( f, ff )
                        ]

                    else if f.name == playerName then
                        [ ( a, aa )
                        , ( b, bb )
                        , ( c, cc )
                        , ( d, dd )
                        , ( e, ee )
                        , ( f, { ff | showed = not ff.showed } )
                        ]

                    else
                        [ ( a, aa )
                        , ( b, bb )
                        , ( c, cc )
                        , ( d, dd )
                        , ( e, ee )
                        , ( f, ff )
                        ]

                _ ->
                    []
    in
    newTeams


updateTeamsWithNewMission playerName newMission teams =
    let
        newTeams =
            case teams of
                [ ( a, aa ), ( b, bb ), ( c, cc ), ( d, dd ), ( e, ee ), ( f, ff ) ] ->
                    if aa.name == playerName then
                        [ ( { a | mission = newMission }, { aa | mission = newMission } )
                        , ( b, bb )
                        , ( c, cc )
                        , ( d, dd )
                        , ( e, ee )
                        , ( f, ff )
                        ]

                    else if bb.name == playerName then
                        [ ( a, aa )
                        , ( { b | mission = newMission }, { bb | mission = newMission } )
                        , ( c, cc )
                        , ( d, dd )
                        , ( e, ee )
                        , ( f, ff )
                        ]

                    else if cc.name == playerName then
                        [ ( a, aa )
                        , ( b, bb )
                        , ( { c | mission = newMission }, { cc | mission = newMission } )
                        , ( d, dd )
                        , ( e, ee )
                        , ( f, ff )
                        ]

                    else if dd.name == playerName then
                        [ ( a, aa )
                        , ( b, bb )
                        , ( c, cc )
                        , ( { d | mission = newMission }, { dd | mission = newMission } )
                        , ( e, ee )
                        , ( f, ff )
                        ]

                    else if ee.name == playerName then
                        [ ( a, aa )
                        , ( b, bb )
                        , ( c, cc )
                        , ( d, dd )
                        , ( { e | mission = newMission }, { ee | mission = newMission } )
                        , ( f, ff )
                        ]

                    else if ff.name == playerName then
                        [ ( a, aa )
                        , ( b, bb )
                        , ( c, cc )
                        , ( d, dd )
                        , ( e, ee )
                        , ( { f | mission = newMission }, { ff | mission = newMission } )
                        ]

                    else
                        [ ( a, aa )
                        , ( b, bb )
                        , ( c, cc )
                        , ( d, dd )
                        , ( e, ee )
                        , ( f, ff )
                        ]

                _ ->
                    []
    in
    newTeams


init : Flags -> ( Model, Cmd msg )
init _ =
    ( { players =
            [ Player blue "blue" False ""
            , Player darkYellow "darkYellow" False ""
            , Player green "green" False ""
            , Player magenta "magenta" False ""
            , Player black "black" False ""
            , Player darkRed "darkRed" False ""
            ]
      , teams = []
      }
    , Cmd.none
    )


main : Program Flags Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Flags =
    ()


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
