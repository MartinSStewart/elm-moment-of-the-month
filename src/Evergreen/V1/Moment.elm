module Evergreen.V1.Moment exposing (..)

import AssocList
import AssocSet
import Evergreen.V1.Id
import String.Nonempty
import Time


type MomentId
    = MomentId Evergreen.V1.Id.UserId Int


type Moment
    = Moment
        { creationTime : Time.Posix
        , content : String.Nonempty.NonemptyString
        , column : Int
        , row : Int
        }


type alias MomentSession =
    { questions : AssocList.Dict MomentId Moment
    , name : String.Nonempty.NonemptyString
    , userId : Evergreen.V1.Id.UserId
    }


type alias BackendMomentSession =
    { questions : AssocList.Dict MomentId Moment
    , host : AssocSet.Set Evergreen.V1.Id.SessionId
    , hostSecret : Evergreen.V1.Id.CryptographicKey Evergreen.V1.Id.HostSecret
    , creationTime : Time.Posix
    , name : String.Nonempty.NonemptyString
    , connections : AssocSet.Set Evergreen.V1.Id.ClientId
    , userIds : AssocList.Dict Evergreen.V1.Id.SessionId Evergreen.V1.Id.UserId
    , connectionCounter : Int
    }
