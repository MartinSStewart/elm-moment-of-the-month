module Network exposing (Change(..), ChangeId, NetworkModel, confirmLocalChange, incrementChangeId, init, initChangeId, localState, serverChange, updateFromUser)

import List.Extra


type NetworkModel local confirmLocal server model
    = NetworkModel { localMsgs : List ( ChangeId, local ), serverState : model }


init : model -> NetworkModel local confirmLocal server model
init model =
    NetworkModel { localMsgs = [], serverState = model }


updateFromUser :
    ChangeId
    -> local
    -> NetworkModel local confirmLocal server model
    -> NetworkModel local confirmLocal server model
updateFromUser changeId localMsg (NetworkModel localModel) =
    { localMsgs = localModel.localMsgs ++ [ ( changeId, localMsg ) ]
    , serverState = localModel.serverState
    }
        |> NetworkModel


localState :
    (Change local confirmLocal server -> model -> model)
    -> NetworkModel local confirmLocal server model
    -> model
localState updateFunc (NetworkModel localModel) =
    List.foldl
        updateFunc
        localModel.serverState
        (List.map (\( changeId, localMsg ) -> LocalChange changeId localMsg) localModel.localMsgs)


serverChange :
    (Change local confirmLocal server -> model -> model)
    -> server
    -> NetworkModel local confirmLocal server model
    -> NetworkModel local confirmLocal server model
serverChange updateFunc serverMsg (NetworkModel localModel) =
    { localMsgs = localModel.localMsgs
    , serverState = updateFunc (ServerChange serverMsg) localModel.serverState
    }
        |> NetworkModel


confirmLocalChange :
    (Change local confirmLocal server -> model -> model)
    -> ChangeId
    -> confirmLocal
    -> NetworkModel local confirmLocal server model
    -> NetworkModel local confirmLocal server model
confirmLocalChange updateFunc changeId confirmLocalMsg (NetworkModel localModel) =
    case List.Extra.find (Tuple.first >> (==) changeId) localModel.localMsgs of
        Just ( _, localMsg ) ->
            { localMsgs = List.filter (\( changeId_, _ ) -> changeId_ /= changeId) localModel.localMsgs
            , serverState =
                updateFunc
                    (ConfirmLocalChange changeId localMsg confirmLocalMsg)
                    localModel.serverState
            }
                |> NetworkModel

        Nothing ->
            NetworkModel localModel


type ChangeId
    = ChangeId Int


initChangeId : ChangeId
initChangeId =
    ChangeId 0


incrementChangeId : ChangeId -> ChangeId
incrementChangeId (ChangeId changeId_) =
    changeId_ + 1 |> ChangeId


type Change local confirmLocal server
    = LocalChange ChangeId local
    | ServerChange server
    | ConfirmLocalChange ChangeId local confirmLocal
