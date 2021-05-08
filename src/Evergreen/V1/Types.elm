module Evergreen.V1.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Evergreen.V1.Id
import Evergreen.V1.Moment
import Evergreen.V1.Network
import Pixels
import Quantity
import String.Nonempty
import Time
import Url


type Key
    = FakeKey
    | ActualKey Browser.Navigation.Key


type LocalQnaMsg
    = CreateQuestion Time.Posix String.Nonempty.NonemptyString
    | DeleteQuestion Evergreen.V1.Moment.MomentId


type ConfirmLocalQnaMsg
    = CreateQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = NewQuestion Evergreen.V1.Moment.MomentId Time.Posix String.Nonempty.NonemptyString
    | QuestionDeleted Evergreen.V1.Moment.MomentId


type alias InQnaSession_ =
    { qnaSessionId : Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.QnaSessionId
    , networkModel : Evergreen.V1.Network.NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg Evergreen.V1.Moment.MomentSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : Evergreen.V1.Network.ChangeId
    , copiedHostUrl : Maybe Time.Posix
    , copiedUrl : Maybe Time.Posix
    , isHost : Maybe (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.HostSecret)
    , lastHeightChange : Maybe Time.Posix
    }


type FrontendStatus
    = Homepage
    | LoadingQnaSession (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.QnaSessionId)
    | LoadingQnaSessionWithHostInvite (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.HostSecret)
    | CreatingQnaSession String.Nonempty.NonemptyString
    | LoadingQnaSessionFailed ()
    | InQnaSession InQnaSession_


type alias FrontendModel =
    { key : Key
    , remoteData : FrontendStatus
    , currentTime : Maybe Time.Posix
    , lastConnectionCheck : Maybe Time.Posix
    , gotFirstConnectMsg : Bool
    , addedRowLastFrame : Bool
    , windowSize : ( Quantity.Quantity Int Pixels.Pixels, Quantity.Quantity Int Pixels.Pixels )
    }


type alias BackendModel =
    { qnaSessions : AssocList.Dict (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.QnaSessionId) Evergreen.V1.Moment.BackendMomentSession
    , keyCounter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | RemoveTemporaryViewOffset
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | GotCurrentTime Time.Posix
    | PressedDeleteQuestion Evergreen.V1.Moment.MomentId
    | PressedCopyHostUrl
    | PressedCopyUrl
    | CheckIfConnected Time.Posix
    | WindowResized ( Quantity.Quantity Int Pixels.Pixels, Quantity.Quantity Int Pixels.Pixels )
    | GotWindowSize ( Quantity.Quantity Int Pixels.Pixels, Quantity.Quantity Int Pixels.Pixels )


type ToBackend
    = LocalMsgRequest (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.QnaSessionId) Evergreen.V1.Network.ChangeId LocalQnaMsg
    | GetQnaSession (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.QnaSessionId)
    | GetQnaSessionWithHostInvite (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.HostSecret)
    | CreateQnaSession String.Nonempty.NonemptyString
    | CheckIfConnectedRequest


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime Evergreen.V1.Id.SessionId Evergreen.V1.Id.ClientId ToBackend Time.Posix
    | UserDisconnected Evergreen.V1.Id.SessionId Evergreen.V1.Id.ClientId
    | UserConnected Evergreen.V1.Id.SessionId Evergreen.V1.Id.ClientId


type ToFrontend
    = ServerMsgResponse (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.QnaSessionId) Evergreen.V1.Network.ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse
        (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.QnaSessionId)
        (Result
            ()
            { isHost : Maybe (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.HostSecret)
            , qnaSession : Evergreen.V1.Moment.MomentSession
            }
        )
    | GetQnaSessionWithHostInviteResponse (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.HostSecret) (Result () ( Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.QnaSessionId, Evergreen.V1.Moment.MomentSession ))
    | CreateQnaSessionResponse (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.QnaSessionId) (Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.HostSecret)
    | CheckIfConnectedResponse
    | NewConnection
