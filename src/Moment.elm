module Moment exposing
    ( BackendMomentSession
    , HostStatus(..)
    , Moment
    , MomentId(..)
    , MomentSession
    , addMoment
    , backendToFrontend
    , currentRow
    , fontSize
    , init
    , initBackend
    , isCreator
    , maxColumn
    , momentColor
    , momentColumn
    , momentContent
    , momentCreationTime
    , momentHeight
    , momentRow
    , momentWidth
    )

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Element
import Id exposing (ClientId, CryptographicKey, HostSecret, SessionId, UserId(..))
import List.Extra as List
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Random
import String.Nonempty exposing (NonemptyString)
import Time


type alias MomentSession =
    { questions : Dict MomentId Moment
    , name : NonemptyString
    , userId : UserId
    }


addMoment :
    MomentId
    -> Time.Posix
    -> NonemptyString
    -> { a | questions : Dict MomentId Moment }
    -> { a | questions : Dict MomentId Moment }
addMoment momentId creationTime content momentSession =
    let
        currentRow_ : Int
        currentRow_ =
            currentRow momentSession

        columnStart : Int
        columnStart =
            getRow currentRow_ momentSession
                |> List.maximumBy (Tuple.second >> momentColumn)
                |> Maybe.map
                    (\( _, moment ) ->
                        momentColumn moment + momentWidth moment
                    )
                |> Maybe.withDefault 0

        columnEnd : Int
        columnEnd =
            columnStart + contentWidth content
    in
    { momentSession
        | questions =
            Dict.insert
                momentId
                (Moment
                    { creationTime = creationTime
                    , content = content
                    , column =
                        if columnEnd > maxColumn then
                            0

                        else
                            columnStart
                    , row =
                        if columnEnd > maxColumn then
                            currentRow_ + 1

                        else
                            currentRow_
                    }
                )
                momentSession.questions
    }


currentRow : { a | questions : Dict MomentId Moment } -> Int
currentRow momentSession =
    Dict.values momentSession.questions
        |> List.maximumBy momentRow
        |> Maybe.map momentRow
        |> Maybe.withDefault 0


maxColumn : Int
maxColumn =
    6


type Moment
    = Moment
        { creationTime : Time.Posix
        , content : NonemptyString
        , column : Int
        , row : Int
        }


randomGrayColor =
    Random.uniform
        0.73
        [ 0.79
        , 0.81
        , 0.85
        ]
        |> Random.map (\a -> Element.rgb a a a)


momentColor : Moment -> Element.Color
momentColor moment =
    (momentRow moment * 17 + momentColumn moment * 5)
        |> Random.initialSeed
        |> Random.step randomGrayColor
        |> Tuple.first


momentRow : Moment -> Int
momentRow (Moment moment) =
    moment.row


momentColumn : Moment -> Int
momentColumn (Moment moment) =
    moment.column


momentContent : Moment -> NonemptyString
momentContent (Moment moment) =
    moment.content


momentWidth : Moment -> Int
momentWidth moment =
    contentWidth (momentContent moment)


momentCreationTime : Moment -> Time.Posix
momentCreationTime (Moment moment) =
    moment.creationTime


fontSize : Bool -> Moment -> Int
fontSize isMobile moment =
    let
        contentLength =
            momentContent moment |> String.Nonempty.length
    in
    if contentLength > 180 then
        if isMobile then
            10

        else
            12

    else if contentLength > 150 then
        if isMobile then
            12

        else
            14

    else if contentLength > 100 then
        if isMobile then
            14

        else
            16

    else if contentLength > 12 then
        if isMobile then
            14

        else
            20

    else if isMobile then
        18

    else
        24


contentWidth : NonemptyString -> number
contentWidth content =
    if String.Nonempty.length content > 50 then
        3

    else if String.Nonempty.length content > 10 then
        2

    else
        1


momentHeight : Bool -> Quantity Int Pixels
momentHeight isMobile =
    if isMobile then
        Pixels.pixels 120

    else
        Pixels.pixels 100


type MomentId
    = MomentId UserId Int


isCreator : UserId -> MomentId -> Bool
isCreator userId (MomentId userId_ _) =
    userId == userId_


getRow : Int -> { a | questions : Dict MomentId Moment } -> List ( MomentId, Moment )
getRow row momentSession =
    Dict.toList momentSession.questions
        |> List.filter (Tuple.second >> momentRow >> (==) row)
        |> List.sortBy (Tuple.second >> momentColumn)


type HostStatus
    = IsHostButLoading
    | IsHost (CryptographicKey HostSecret)
    | IsNotHost


type alias BackendMomentSession =
    { questions : Dict MomentId Moment
    , host : Set SessionId
    , hostSecret : CryptographicKey HostSecret
    , creationTime : Time.Posix
    , name : NonemptyString
    , connections : Set ClientId
    , userIds : Dict SessionId UserId
    , connectionCounter : Int
    }


init : NonemptyString -> MomentSession
init name =
    { questions = Dict.empty
    , name = name
    , userId = UserId 0
    }


initBackend :
    SessionId
    -> ClientId
    -> CryptographicKey HostSecret
    -> Time.Posix
    -> NonemptyString
    -> BackendMomentSession
initBackend hostSessionId hostClientId hostSecret creationTime name =
    { questions = Dict.empty
    , host = Set.singleton hostSessionId
    , hostSecret = hostSecret
    , creationTime = creationTime
    , name = name
    , connections = Set.singleton hostClientId
    , userIds = Dict.singleton hostSessionId (UserId 0)
    , connectionCounter = 1
    }


backendToFrontend : UserId -> BackendMomentSession -> MomentSession
backendToFrontend userId qnaSession =
    { questions = qnaSession.questions
    , name = qnaSession.name
    , userId = userId
    }
