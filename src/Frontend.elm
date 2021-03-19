module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Color.OneDark as OneDark exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes as Attr
import Lamdera
import Random
import Random.List
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
      , players =
            [ Player Blue "Bleu" False ""
            , Player DarkYellow "Jaune" False ""
            , Player Green "Vert" False ""
            , Player Magenta "Magenta" False ""
            , Player Black "Noir" False ""
            , Player DarkRed "Rouge" False ""
            ]
      , teams = []
      , missions =
            [ "Capturer l'Europe, l'Australie et un autre continent"
            , "Capturer l'Europe, l'Amérique du Sud et un autre continent"
            , "Capturer l'Amérique du Nord et l'Afrique"
            , "Capturer l'Asie et l'Amérique du Sud"
            , "Capturer l'Amérique du Nord et l'Australie"
            , "Capturer 24 territoires"
            , "Détruire toutes les armées des bleus ou, dans le cas d'être soi-même le joueur nommé, capturer 24 territoires"
            , "Détruire toutes les armées des jaunes ou, dans le cas où vous êtes vous-même le joueur nommé, capturer 24 territoires"
            , "Détruire toutes les armées des verts ou, si l'on est soi-même le joueur désigné, capturer 24 territoires"
            , "Détruire toutes les armées des magentas ou, si l'on est soi-même le joueur désigné, capturer 24 territoires"
            , "Détruire toutes les armées de noirs ou, si l'on est soi-même le joueur désigné, capturer 24 territoires"
            , "Détruire toutes les armées des rouges ou, si l'on est soi-même le joueur désigné, capturer 24 territoires"
            , "Capturer 18 territoires et les occuper avec deux troupes minimum"
            ]
      , hasBegun = False
      , teamOpened = Nothing
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        CheckboxClicked value ->
            ( model, Cmd.none )

        RadioClicked value ->
            ( model, Cmd.none )

        TextChanged value ->
            ( model, Cmd.none )

        BeginGame ->
            ( { model | hasBegun = True }
            , Random.generate ReceiveRandomPlayers (Random.List.shuffle model.players)
            )

        ReceiveRandomPlayers newList ->
            let
                teams =
                    case newList of
                        [ a, b, c, d, e, f ] ->
                            [ ( a, b ), ( c, d ), ( e, f ), ( b, a ), ( d, c ), ( f, e ) ]

                        _ ->
                            []
            in
            ( { model | players = newList, teams = teams }
            , Random.generate ReceiveRandomMissions (Random.List.shuffle model.missions)
            )

        ReceiveRandomMissions newList ->
            ( { model | missions = newList, players = fillMissionToPlayers model.players newList, teams = fillMissionsToTeams (Debug.log "Old TEAMS:" model.teams) newList }, Cmd.none )

        ShowTeamMate playerName ->
            ( { model
                | teams = updateTeamsWithShowed playerName model.teams
                , teamOpened =
                    case model.teamOpened of
                        Just teamOpened ->
                            Nothing

                        Nothing ->
                            Debug.log "sadas" <| Just playerName
              }
            , Cmd.none
            )

        ChangeMission playerName newMissionInput ->
            ( { model | teams = updateTeamsWithNewMission playerName newMissionInput model.teams }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view model =
    { title = ""
    , body =
        [ Element.layout
            [ Element.width Element.fill, Element.centerX ]
            (Element.column
                [ Background.color <| rgba255 255 255 255 1
                , Background.image
                    "https://cdn.pixabay.com/photo/2019/06/30/20/55/board-of-risk-4308773_1280.png"
                , Font.color <| rgba255 46 52 54 1
                , Font.family
                    [ Font.typeface "system-ui"
                    , Font.typeface "-apple-system"
                    , Font.typeface "sans-serif"
                    ]
                , Font.size 12
                , height fill
                , width fill
                ]
                [ if not model.hasBegun then
                    Element.paragraph
                        [ Font.center
                        , Font.letterSpacing 5
                        , Font.bold
                        , Font.color <| rgba255 46 52 54 1
                        , Font.size 36
                        , Element.height Element.shrink
                        , Element.width Element.fill
                        , Element.paddingXY 16 16
                        , Region.heading 1
                        ]
                        [ Element.text "Teams-Risk" ]

                  else
                    none
                , if not model.hasBegun then
                    Input.button
                        [ Background.color <| rgba255 52 101 164 1
                        , Element.centerY
                        , Element.centerX
                        , Font.center
                        , Font.color <| rgba255 255 255 255 1
                        , Font.size 30
                        , Element.height Element.shrink
                        , Element.width Element.shrink
                        , Element.paddingXY 16 16
                        , Border.rounded 2
                        , Border.color <| rgba255 52 101 164 1
                        , Border.solid
                        , Border.widthXY 1 1
                        ]
                        { onPress = Just BeginGame
                        , label = Element.text "\u{1F977} GO 🧙\u{200D}♂️"
                        }

                  else
                    none
                , el [ centerX, centerY, paddingXY 32 32 ] <| showTeams model.teams model.teamOpened
                ]
            )
        ]
    }


showTeams : List ( Player, Player ) -> Maybe String -> Element FrontendMsg
showTeams teams teamOpened =
    wrappedRow
        [ centerX
        , width fill
        , height fill
        , spacing 22
        ]
        (List.map (showTeam teamOpened) teams)


showTeam : Maybe String -> ( Player, Player ) -> Element FrontendMsg
showTeam teamOpened team =
    case teamOpened of
        Just playerName ->
            if (.name <| Tuple.first team) == playerName then
                column
                    [ spacing 22
                    ]
                    [ showPlayer <| Tuple.first team, showMissionsIfClicked (.showed <| Tuple.second team) (Tuple.first team), showPlayerIfClicked <| Tuple.second team, showMissionIfClicked <| Tuple.second team ]

            else
                none

        Nothing ->
            column
                [ spacing 22
                ]
                [ showPlayer <| Tuple.first team, showMissionsIfClicked (.showed <| Tuple.second team) (Tuple.first team), showPlayerIfClicked <| Tuple.second team, showMissionIfClicked <| Tuple.second team ]


showMissionIfClicked : Player -> Element FrontendMsg
showMissionIfClicked player =
    if player.showed then
        column [ width <| px 140, height <| px 140, Background.color <| playerColorToElementColor player.color, Border.rounded 9, paddingXY 12 12 ]
            [ el [ centerX, centerY ] <|
                paragraph
                    [ Font.color
                        (if player.color == Black then
                            OneDark.white

                         else
                            OneDark.black
                        )
                    ]
                    [ text player.mission ]
            ]

    else
        none


showMissionsIfClicked : Bool -> Player -> Element FrontendMsg
showMissionsIfClicked isOpen player =
    if isOpen then
        column [ width <| px 140, height <| px 140, Background.color <| playerColorToElementColor player.color, Border.rounded 9, paddingXY 12 12 ]
            [ el [ centerX, centerY ] <|
                paragraph
                    [ Font.color <|
                        playerColorToElementColor
                            (if player.color == Black then
                                White

                             else
                                Black
                            )
                    ]
                    [ text player.mission ]
            ]

    else
        none


showPlayerIfClicked player =
    if player.showed then
        column [ width <| px 140, height <| px 140, Background.color <| playerColorToElementColor player.color, Border.rounded 9, paddingXY 12 12 ]
            [ el
                [ centerX
                , centerY
                , Font.size 22
                , Font.color <|
                    playerColorToElementColor
                        (if player.color == Black then
                            White

                         else
                            Black
                        )
                ]
              <|
                text player.name
            ]

    else
        none


showPlayer : Player -> Element FrontendMsg
showPlayer player =
    column
        [ Events.onClick <| ShowTeamMate player.name
        , pointer
        , width <| px 140
        , height <| px 140
        , Background.color <| playerColorToElementColor player.color
        , Border.rounded 9
        ]
        [ el
            [ centerX
            , centerY
            , Font.size 22
            , Font.color <|
                playerColorToElementColor
                    (if player.color == Black then
                        White

                     else
                        Black
                    )
            ]
          <|
            text player.name
        ]


fillMissionsToTeams : List ( Player, Player ) -> List String -> List ( Player, Player )
fillMissionsToTeams teams missions =
    case ( teams, missions ) of
        ( [ ( a, b ), ( c, d ), ( e, f ), ( bb, aa ), ( dd, cc ), ( ff, ee ) ], ma :: mb :: mc :: md :: me :: mf :: l ) ->
            [ ( { a | mission = ma }, { b | mission = mb } )
            , ( { c | mission = mc }, { d | mission = md } )
            , ( { e | mission = me }, { f | mission = mf } )
            , ( { bb | mission = mb }, { aa | mission = ma } )
            , ( { dd | mission = md }, { cc | mission = mc } )
            , ( { ff | mission = mf }, { ee | mission = me } )
            ]

        _ ->
            teams


fillMissionToPlayers : List Player -> List String -> List Player
fillMissionToPlayers players missions =
    Debug.log "newPlayerList: " <| List.map2 fillMissionToPlayer players missions


fillMissionToPlayer player mission =
    { player | mission = mission }


updateTeamsWithShowed playerName teams =
    let
        newTeams =
            case teams of
                [ ( a, aa ), ( b, bb ), ( c, cc ), ( d, dd ), ( e, ee ), ( f, ff ) ] ->
                    if a.name == playerName then
                        [ ( a, { aa | showed = not aa.showed } )
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


playerColorToElementColor : PlayerColor -> Element.Color
playerColorToElementColor playerColor =
    case playerColor of
        Blue ->
            blue

        DarkYellow ->
            darkYellow

        Green ->
            green

        Magenta ->
            magenta

        Black ->
            black

        DarkRed ->
            darkRed

        White ->
            white


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
