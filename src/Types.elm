module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Element


type alias FrontendModel =
    { key : Key
    , message : String
    , players : List Player
    , teams : List ( Player, Player )
    , missions : List String
    , hasBegun : Bool
    , teamOpened : Maybe String
    }


type alias Player =
    { color : Element.Color
    , name : String
    , showed : Bool
    , mission : String
    }

type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | CheckboxClicked Bool
    | RadioClicked Int
    | TextChanged String
    | BeginGame
    | ReceiveRandomPlayers (List Player)
    | ReceiveRandomMissions (List String)
    | ShowTeamMate String
    | ChangeMission String String

type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
