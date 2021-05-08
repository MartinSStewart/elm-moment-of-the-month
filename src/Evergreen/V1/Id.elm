module Evergreen.V1.Id exposing (..)

import Lamdera


type QnaSessionId
    = QnaSessionId Never


type CryptographicKey a
    = CryptographicKey String


type HostSecret
    = HostSecret Never


type UserId
    = UserId Int


type SessionId
    = SessionId Lamdera.SessionId


type ClientId
    = ClientId Lamdera.ClientId
