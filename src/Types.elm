module Types exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest)
import Browser.Navigation
import Duration exposing (Duration)
import Id exposing (ClientId, CryptographicKey, HostSecret, QnaSessionId, SessionId, UserId(..))
import Moment exposing (BackendMomentSession, HostStatus, MomentId(..), MomentSession)
import Network exposing (ChangeId, NetworkModel)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import String.Nonempty exposing (NonemptyString)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , remoteData : FrontendStatus
    , currentTime : Maybe Time.Posix
    , lastConnectionCheck : Maybe Time.Posix
    , gotFirstConnectMsg : Bool
    , addedRowLastFrame : Bool
    , windowSize : ( Quantity Int Pixels, Quantity Int Pixels )
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
    , networkModel : NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg MomentSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : ChangeId
    , copiedHostUrl : Maybe Time.Posix
    , copiedUrl : Maybe Time.Posix
    , isHost : Maybe (CryptographicKey HostSecret)
    }


initInQnaSession : CryptographicKey QnaSessionId -> MomentSession -> Maybe (CryptographicKey HostSecret) -> InQnaSession_
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
    { qnaSessions : Dict (CryptographicKey QnaSessionId) BackendMomentSession
    , keyCounter : Int
    }


getQuestionId : Dict MomentId v -> UserId -> MomentId
getQuestionId questions userId =
    Dict.filter
        (\(MomentId userId_ _) _ -> userId_ == userId)
        questions
        |> Dict.size
        |> MomentId userId


type LocalQnaMsg
    = CreateQuestion Time.Posix NonemptyString
    | DeleteQuestion MomentId


type ConfirmLocalQnaMsg
    = CreateQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = NewQuestion MomentId Time.Posix NonemptyString
    | QuestionDeleted MomentId


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | RemoveTemporaryViewOffset
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | GotCurrentTime Time.Posix
    | PressedDeleteQuestion MomentId
    | PressedCopyHostUrl
    | PressedCopyUrl
    | CheckIfConnected Time.Posix
    | WindowResized ( Quantity Int Pixels, Quantity Int Pixels )
    | GotWindowSize ( Quantity Int Pixels, Quantity Int Pixels )


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


type ToFrontend
    = ServerMsgResponse (CryptographicKey QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (CryptographicKey QnaSessionId) ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse
        (CryptographicKey QnaSessionId)
        (Result
            ()
            { isHost : Maybe (CryptographicKey HostSecret)
            , qnaSession : MomentSession
            }
        )
    | GetQnaSessionWithHostInviteResponse (CryptographicKey HostSecret) (Result () ( CryptographicKey QnaSessionId, MomentSession ))
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
    | ScrollEffect
    | Blur String
    | GetWindowSize (( Quantity Int Pixels, Quantity Int Pixels ) -> FrontendMsg)


type BackendSub
    = SubBatch (List BackendSub)
    | TimeEvery Duration (Time.Posix -> BackendMsg)
    | ClientDisconnected (SessionId -> ClientId -> BackendMsg)
    | ClientConnected (SessionId -> ClientId -> BackendMsg)


type FrontendSub
    = SubBatch_ (List FrontendSub)
    | TimeEvery_ Duration (Time.Posix -> FrontendMsg)
    | WindowResize (( Quantity Int Pixels, Quantity Int Pixels ) -> FrontendMsg)
