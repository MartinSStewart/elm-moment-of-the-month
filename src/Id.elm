module Id exposing (ClientId(..), CryptographicKey(..), HostSecret, QnaSessionId, SessionId(..), UserId(..), crytographicKeyToString, getShortCryptographicKey)

import Env
import Lamdera
import Sha256


type ClientId
    = ClientId Lamdera.ClientId


type SessionId
    = SessionId Lamdera.SessionId


type QnaSessionId
    = QnaSessionId Never


type HostSecret
    = HostSecret Never


type CryptographicKey a
    = CryptographicKey String


type UserId
    = UserId Int


getShortCryptographicKey : { a | keyCounter : Int } -> ( { a | keyCounter : Int }, CryptographicKey b )
getShortCryptographicKey model =
    ( { model | keyCounter = model.keyCounter + 1 }
    , Env.secretKey ++ String.fromInt model.keyCounter |> Sha256.sha224 |> String.left 8 |> CryptographicKey
    )


crytographicKeyToString : CryptographicKey a -> String
crytographicKeyToString (CryptographicKey key) =
    key
