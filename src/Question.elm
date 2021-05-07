module Question exposing (..)

import AssocSet as Set exposing (Set)
import Id exposing (SessionId, UserId(..))
import String.Nonempty exposing (NonemptyString)
import Time


type alias Question =
    { creationTime : Time.Posix
    , content : NonemptyString
    , isPinned : Maybe Time.Posix
    , otherVotes : Int
    , isUpvoted : Bool
    }


type QuestionId
    = QuestionId UserId Int


type alias BackendQuestion =
    { creationTime : Time.Posix
    , content : NonemptyString
    , isPinned : Maybe Time.Posix
    , votes : Set SessionId
    }


votes : Question -> Int
votes question =
    if question.isUpvoted then
        question.otherVotes + 1

    else
        question.otherVotes


backendToFrontend : SessionId -> BackendQuestion -> Question
backendToFrontend sessionId backendQuestion =
    { creationTime = backendQuestion.creationTime
    , content = backendQuestion.content
    , isPinned = backendQuestion.isPinned
    , otherVotes = Set.remove sessionId backendQuestion.votes |> Set.size
    , isUpvoted = Set.member sessionId backendQuestion.votes
    }


isNewQuestion : Time.Posix -> Question -> Bool
isNewQuestion currentTime question =
    Time.posixToMillis question.creationTime + 1600 > Time.posixToMillis currentTime


isCreator : UserId -> QuestionId -> Bool
isCreator userId (QuestionId userId_ _) =
    userId == userId_


questionIdToString : QuestionId -> String
questionIdToString (QuestionId (UserId userId) questionIndex) =
    String.fromInt userId ++ " " ++ String.fromInt questionIndex
