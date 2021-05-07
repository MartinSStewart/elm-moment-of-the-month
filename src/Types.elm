module Types exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest)
import Browser.Navigation
import Duration exposing (Duration)
import Id exposing (ClientId, CryptographicKey, HostSecret, QnaSessionId, SessionId, UserId(..))
import Network exposing (ChangeId, NetworkModel)
import QnaSession exposing (BackendQnaSession, HostStatus, QnaSession)
import Question exposing (BackendQuestion, Question, QuestionId(..))
import String.Nonempty exposing (NonemptyString)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , remoteData : FrontendStatus
    , currentTime : Maybe Time.Posix
    , lastConnectionCheck : Maybe Time.Posix
    , gotFirstConnectMsg : Bool
    }


type Key
    = FakeKey
    | ActualKey Browser.Navigation.Key


type FrontendStatus
    = Homepage
    | LoadingQnaSession (CryptographicKey QnaSessionId)
    | LoadingQnaSessionWithHostInvite (CryptographicKey HostSecret)
    | CreatingQnaSession NonemptyString
    | LoadingQnaSessionFailed ()
    | InQnaSession InQnaSession_


type alias InQnaSession_ =
    { qnaSessionId : CryptographicKey QnaSessionId
    , networkModel : NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : ChangeId
    , copiedHostUrl : Maybe Time.Posix
    , copiedUrl : Maybe Time.Posix
    , isHost : Maybe (CryptographicKey HostSecret)
    }


initInQnaSession : CryptographicKey QnaSessionId -> QnaSession -> Maybe (CryptographicKey HostSecret) -> InQnaSession_
initInQnaSession qnaSessionId qnaSesssion hostStatus =
    { qnaSessionId = qnaSessionId
    , networkModel = Network.init qnaSesssion
    , question = ""
    , pressedCreateQuestion = False
    , localChangeCounter = Network.initChangeId
    , copiedHostUrl = Nothing
    , copiedUrl = Nothing
    , isHost = hostStatus
    }


type alias BackendModel =
    { qnaSessions : Dict (CryptographicKey QnaSessionId) BackendQnaSession
    , keyCounter : Int
    }


getQuestionId : Dict QuestionId v -> UserId -> QuestionId
getQuestionId questions userId =
    Dict.filter
        (\(QuestionId userId_ _) _ -> userId_ == userId)
        questions
        |> Dict.size
        |> QuestionId userId


type LocalQnaMsg
    = ToggleUpvote QuestionId
    | CreateQuestion Time.Posix NonemptyString
    | TogglePin QuestionId Time.Posix
    | DeleteQuestion QuestionId


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = VoteAdded QuestionId
    | VoteRemoved QuestionId
    | NewQuestion QuestionId Time.Posix NonemptyString
    | QuestionPinned QuestionId (Maybe Time.Posix)
    | QuestionDeleted QuestionId


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote QuestionId
    | PressedTogglePin QuestionId
    | GotCurrentTime Time.Posix
    | PressedDownloadQuestions
    | PressedDeleteQuestion QuestionId
    | PressedCopyHostUrl
    | PressedCopyUrl
    | CheckIfConnected Time.Posix


type ToBackend
    = LocalMsgRequest (CryptographicKey QnaSessionId) ChangeId LocalQnaMsg
    | GetQnaSession (CryptographicKey QnaSessionId)
    | GetQnaSessionWithHostInvite (CryptographicKey HostSecret)
    | CreateQnaSession NonemptyString
    | CheckIfConnectedRequest


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime SessionId ClientId ToBackend Time.Posix
    | UserDisconnected SessionId ClientId
    | UserConnected SessionId ClientId
    | CheckSessions Time.Posix


type ToFrontend
    = ServerMsgResponse (CryptographicKey QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (CryptographicKey QnaSessionId) ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse
        (CryptographicKey QnaSessionId)
        (Result
            ()
            { isHost : Maybe (CryptographicKey HostSecret)
            , qnaSession : QnaSession
            }
        )
    | GetQnaSessionWithHostInviteResponse (CryptographicKey HostSecret) (Result () ( CryptographicKey QnaSessionId, QnaSession ))
    | CreateQnaSessionResponse (CryptographicKey QnaSessionId) (CryptographicKey HostSecret)
    | CheckIfConnectedResponse
    | NewConnection


type BackendEffect
    = Batch (List BackendEffect)
    | SendToFrontend ClientId ToFrontend
    | TimeNow (Time.Posix -> BackendMsg)


type FrontendEffect
    = Batch_ (List FrontendEffect)
    | SendToBackend ToBackend
    | PushUrl Key String
    | ReplaceUrl Key String
    | LoadUrl String
    | FileDownload String String String
    | CopyToClipboard String
    | ScrollToBottom String
    | Blur String


type BackendSub
    = SubBatch (List BackendSub)
    | TimeEvery Duration (Time.Posix -> BackendMsg)
    | ClientDisconnected (SessionId -> ClientId -> BackendMsg)
    | ClientConnected (SessionId -> ClientId -> BackendMsg)


type FrontendSub
    = SubBatch_ (List FrontendSub)
    | TimeEvery_ Duration (Time.Posix -> FrontendMsg)
