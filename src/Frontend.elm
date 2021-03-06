port module Frontend exposing
    ( app
    , domain
    , init
    , qnaSessionUpdate
    , subscriptions
    , update
    , updateFromBackend
    , view
    )

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Doodads
import Duration
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Keyed
import Element.Lazy
import File.Download
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Id exposing (CryptographicKey(..), HostSecret, QnaSessionId, UserId(..))
import Json.Decode
import Lamdera
import Moment exposing (Moment, MomentId, MomentSession)
import Network exposing (Change(..), NetworkModel)
import Pixels exposing (Pixels)
import Process
import Quantity exposing (Quantity)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as Property
import String.Nonempty as NonemptyString exposing (NonemptyString(..))
import Task exposing (Task)
import Time
import Types exposing (ConfirmLocalQnaMsg(..), FrontendEffect(..), FrontendModel, FrontendMsg(..), FrontendStatus(..), FrontendSub(..), InQnaSession_, Key(..), LocalQnaMsg(..), ServerQnaMsg(..), ToBackend(..), ToFrontend(..))
import Url exposing (Url)
import Url.Parser exposing ((</>))


port supermario_copy_to_clipboard_to_js : String -> Cmd msg


app =
    Lamdera.frontend
        { init = \url key -> init url (ActualKey key) |> Tuple.mapSecond effectToCmd
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = \msg model -> update msg model |> Tuple.mapSecond effectToCmd
        , updateFromBackend = \msg model -> updateFromBackend msg model |> Tuple.mapSecond effectToCmd
        , subscriptions = subscriptions >> frontendSubToSub
        , view = view
        }


subscriptions : FrontendModel -> FrontendSub
subscriptions _ =
    SubBatch_
        [ TimeEvery_ Duration.second GotCurrentTime
        , TimeEvery_ (Duration.seconds 10) CheckIfConnected
        , WindowResize WindowResized
        ]


frontendSubToSub : FrontendSub -> Sub FrontendMsg
frontendSubToSub sub =
    case sub of
        SubBatch_ frontendSubs ->
            List.map frontendSubToSub frontendSubs |> Sub.batch

        TimeEvery_ duration msg ->
            Time.every (Duration.inMilliseconds duration) msg

        WindowResize onResize ->
            Browser.Events.onResize (\width height -> onResize ( Pixels.pixels width, Pixels.pixels height ))


effectToCmd : FrontendEffect -> Cmd FrontendMsg
effectToCmd effect =
    case effect of
        Batch_ frontendEffects ->
            List.map effectToCmd frontendEffects |> Cmd.batch

        SendToBackend toBackend ->
            Lamdera.sendToBackend toBackend

        PushUrl key url ->
            case key of
                FakeKey ->
                    Cmd.none

                ActualKey key_ ->
                    Browser.Navigation.pushUrl key_ url

        ReplaceUrl key url ->
            case key of
                FakeKey ->
                    Cmd.none

                ActualKey key_ ->
                    Browser.Navigation.replaceUrl key_ url

        LoadUrl url ->
            Browser.Navigation.load url

        FileDownload name mime content ->
            File.Download.string name mime content

        CopyToClipboard text ->
            supermario_copy_to_clipboard_to_js text

        Blur id ->
            Browser.Dom.blur id |> Task.attempt (always NoOpFrontendMsg)

        GetWindowSize msg ->
            Browser.Dom.getViewport
                |> Task.map
                    (\{ viewport } ->
                        ( Pixels.pixels <| round viewport.width, Pixels.pixels <| round viewport.height )
                    )
                |> Task.perform msg


init : Url.Url -> Key -> ( FrontendModel, FrontendEffect )
init url key =
    (case Url.Parser.parse urlDecoder url of
        Just (QnaSessionRoute qnaSessionId) ->
            qnaSessionRouteInit False key qnaSessionId

        Just (HostInviteRoute hostSecret) ->
            hostInviteRouteInit False key hostSecret

        Just HomepageRoute ->
            homepageRouteInit False key

        Nothing ->
            homepageRouteInit False key
    )
        |> Tuple.mapSecond (\effects -> Batch_ [ effects, GetWindowSize GotWindowSize ])


qnaSessionRouteInit : Bool -> Key -> CryptographicKey QnaSessionId -> ( FrontendModel, FrontendEffect )
qnaSessionRouteInit gotFirstConnectMsg key qnaSessionId =
    ( { key = key
      , remoteData = LoadingQnaSession qnaSessionId
      , currentTime = Nothing
      , lastConnectionCheck = Nothing
      , gotFirstConnectMsg = gotFirstConnectMsg
      , addedRowLastFrame = False
      , windowSize = ( Pixels.pixels 1920, Pixels.pixels 1080 )
      }
    , SendToBackend (GetQnaSession qnaSessionId)
    )


hostInviteRouteInit : Bool -> Key -> CryptographicKey HostSecret -> ( FrontendModel, FrontendEffect )
hostInviteRouteInit gotFirstConnectMsg key hostSecret =
    ( { key = key
      , remoteData = LoadingQnaSessionWithHostInvite hostSecret
      , currentTime = Nothing
      , lastConnectionCheck = Nothing
      , gotFirstConnectMsg = gotFirstConnectMsg
      , addedRowLastFrame = False
      , windowSize = ( Pixels.pixels 1920, Pixels.pixels 1080 )
      }
    , SendToBackend (GetQnaSessionWithHostInvite hostSecret)
    )


homepageRouteInit : Bool -> Key -> ( FrontendModel, FrontendEffect )
homepageRouteInit gotFirstConnectMsg key =
    ( { key = key
      , remoteData = Homepage
      , currentTime = Nothing
      , lastConnectionCheck = Nothing
      , gotFirstConnectMsg = gotFirstConnectMsg
      , addedRowLastFrame = False
      , windowSize = ( Pixels.pixels 1920, Pixels.pixels 1080 )
      }
    , Batch_ []
    )


type Route
    = HomepageRoute
    | HostInviteRoute (CryptographicKey HostSecret)
    | QnaSessionRoute (CryptographicKey QnaSessionId)


urlDecoder : Url.Parser.Parser (Route -> c) c
urlDecoder =
    Url.Parser.oneOf
        [ Url.Parser.top |> Url.Parser.map HomepageRoute
        , Url.Parser.s hostInvite </> Url.Parser.string |> Url.Parser.map (CryptographicKey >> HostInviteRoute)
        , Url.Parser.string |> Url.Parser.map (CryptographicKey >> QnaSessionRoute)
        ]


urlEncoder : CryptographicKey QnaSessionId -> String
urlEncoder (CryptographicKey qnaSessionId) =
    "/" ++ qnaSessionId


update : FrontendMsg -> FrontendModel -> ( FrontendModel, FrontendEffect )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , PushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , LoadUrl url
                    )

        UrlChanged url ->
            case Url.Parser.parse urlDecoder url of
                Just HomepageRoute ->
                    ( model, LoadUrl "/" )

                Just (QnaSessionRoute qnaSessionId) ->
                    case model.remoteData of
                        InQnaSession inQnaSession ->
                            if inQnaSession.qnaSessionId == qnaSessionId then
                                ( model, Batch_ [] )

                            else
                                ( model, LoadUrl (urlEncoder qnaSessionId) )

                        _ ->
                            ( model, LoadUrl (urlEncoder qnaSessionId) )

                _ ->
                    ( model, Batch_ [] )

        NoOpFrontendMsg ->
            ( model, Batch_ [] )

        PressedCreateQnaSession ->
            let
                name =
                    NonemptyString 'T' "est"
            in
            ( { model | remoteData = CreatingQnaSession name }, SendToBackend (CreateQnaSession name) )

        TypedQuestion text ->
            case model.remoteData of
                InQnaSession inQnaSession ->
                    ( { model | remoteData = InQnaSession { inQnaSession | question = text } }, Batch_ [] )

                _ ->
                    ( model, Batch_ [] )

        PressedCreateQuestion ->
            updateInQnaSession
                (\inQnaSession ->
                    case valiatedQuestion inQnaSession.question of
                        Ok nonempty ->
                            let
                                localMsg =
                                    CreateQuestion
                                        (Maybe.withDefault (Time.millisToPosix 0) model.currentTime)
                                        nonempty
                            in
                            ( { inQnaSession
                                | networkModel =
                                    Network.updateFromUser
                                        inQnaSession.localChangeCounter
                                        localMsg
                                        inQnaSession.networkModel
                                , question = ""
                                , pressedCreateQuestion = False
                                , localChangeCounter = Network.incrementChangeId inQnaSession.localChangeCounter
                              }
                            , Batch_
                                [ SendToBackend
                                    (LocalMsgRequest inQnaSession.qnaSessionId inQnaSession.localChangeCounter localMsg)
                                , Blur questionInputId
                                ]
                            )

                        Err _ ->
                            ( { inQnaSession | pressedCreateQuestion = True }
                            , Batch_ []
                            )
                )
                model

        GotCurrentTime currentTime ->
            ( { model
                | currentTime = Just currentTime
                , lastConnectionCheck = Maybe.withDefault currentTime model.lastConnectionCheck |> Just
              }
            , Batch_ []
            )

        PressedDeleteQuestion questionId ->
            updateInQnaSession
                (addLocalChange (DeleteQuestion questionId))
                model

        PressedCopyHostUrl ->
            updateInQnaSession
                (\inQnaSession ->
                    case inQnaSession.isHost of
                        Just hostSecret ->
                            ( { inQnaSession | copiedHostUrl = model.currentTime }
                            , CopyToClipboard ("https://" ++ hostSecretToUrl hostSecret)
                            )

                        Nothing ->
                            ( inQnaSession, Batch_ [] )
                )
                model

        PressedCopyUrl ->
            updateInQnaSession
                (\inQnaSession ->
                    ( { inQnaSession | copiedUrl = model.currentTime }
                    , CopyToClipboard ("https://" ++ domain ++ urlEncoder inQnaSession.qnaSessionId)
                    )
                )
                model

        CheckIfConnected _ ->
            ( model, SendToBackend CheckIfConnectedRequest )

        RemoveTemporaryViewOffset ->
            ( { model | addedRowLastFrame = False }, Batch_ [] )

        WindowResized windowSize ->
            ( { model | windowSize = windowSize }, Batch_ [] )

        GotWindowSize windowSize ->
            ( { model | windowSize = windowSize }, Batch_ [] )


hostSecretToUrl : CryptographicKey HostSecret -> String
hostSecretToUrl hostSecret =
    domain ++ "/" ++ hostInvite ++ "/" ++ Id.crytographicKeyToString hostSecret


domain : String
domain =
    "moment-of-the-month.app"


hostInvite : String
hostInvite =
    "host-invite"


addLocalChange : LocalQnaMsg -> InQnaSession_ -> ( InQnaSession_, FrontendEffect )
addLocalChange localMsg inQnaSession =
    ( { inQnaSession
        | networkModel =
            Network.updateFromUser
                inQnaSession.localChangeCounter
                localMsg
                inQnaSession.networkModel
        , localChangeCounter = Network.incrementChangeId inQnaSession.localChangeCounter
      }
    , SendToBackend (LocalMsgRequest inQnaSession.qnaSessionId inQnaSession.localChangeCounter localMsg)
    )


questionInputId : String
questionInputId =
    "question-input-id"


step : (number -> Float -> Task x a) -> Int -> Float -> Float -> Time.Posix -> Time.Posix -> Task x a
step f millis start end startTime now =
    let
        elapsed : Int
        elapsed =
            Time.posixToMillis now - Time.posixToMillis startTime
    in
    f 0 (position millis start end elapsed)
        |> Task.andThen
            (if elapsed < millis then
                \_ -> Time.now |> Task.andThen (step f millis start end startTime)

             else
                Task.succeed
            )


position : Int -> Float -> Float -> Int -> Float
position millis start end elapsed =
    if elapsed < millis then
        start + (end - start) * (toFloat elapsed / toFloat millis)

    else
        end


updateInQnaSession : (InQnaSession_ -> ( InQnaSession_, FrontendEffect )) -> FrontendModel -> ( FrontendModel, FrontendEffect )
updateInQnaSession updateFunc model =
    case model.remoteData of
        InQnaSession inQnaSession ->
            updateFunc inQnaSession
                |> Tuple.mapFirst (\a -> { model | remoteData = InQnaSession a })

        _ ->
            ( model, Batch_ [] )


deleteQuestion : MomentId -> MomentSession -> MomentSession
deleteQuestion questionId qnaSession =
    { qnaSession | questions = Dict.remove questionId qnaSession.questions }


qnaSessionUpdate : Change LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg -> MomentSession -> MomentSession
qnaSessionUpdate msg qnaSession =
    case msg of
        LocalChange _ (CreateQuestion creationTime content) ->
            let
                questionId : MomentId
                questionId =
                    Types.getQuestionId qnaSession.questions qnaSession.userId
            in
            Moment.addMoment questionId creationTime content qnaSession

        LocalChange _ (DeleteQuestion questionId) ->
            deleteQuestion questionId qnaSession

        ConfirmLocalChange _ localChange (CreateQuestionResponse creationTime) ->
            case localChange of
                CreateQuestion _ content ->
                    let
                        questionId : MomentId
                        questionId =
                            Types.getQuestionId qnaSession.questions qnaSession.userId
                    in
                    Moment.addMoment questionId creationTime content qnaSession

                _ ->
                    qnaSession

        ConfirmLocalChange _ localChange DeleteQuestionResponse ->
            case localChange of
                DeleteQuestion questionId ->
                    deleteQuestion questionId qnaSession

                _ ->
                    qnaSession

        ServerChange (NewQuestion questionId creationTime content) ->
            Moment.addMoment questionId creationTime content qnaSession

        ServerChange (QuestionDeleted questionId) ->
            deleteQuestion questionId qnaSession


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, FrontendEffect )
updateFromBackend msg model =
    case msg of
        ServerMsgResponse qnaSessionId serverQnaMsg ->
            updateInQnaSession
                (\inQnaSession ->
                    ( if inQnaSession.qnaSessionId == qnaSessionId then
                        { inQnaSession
                            | networkModel =
                                Network.serverChange qnaSessionUpdate serverQnaMsg inQnaSession.networkModel
                        }

                      else
                        inQnaSession
                    , Batch_ []
                    )
                )
                model

        LocalConfirmQnaMsgResponse qnaSessionId changeId confirmLocalMsg ->
            updateInQnaSession
                (\inQnaSession ->
                    ( if inQnaSession.qnaSessionId == qnaSessionId then
                        { inQnaSession
                            | networkModel =
                                Network.confirmLocalChange
                                    qnaSessionUpdate
                                    changeId
                                    confirmLocalMsg
                                    inQnaSession.networkModel
                        }

                      else
                        inQnaSession
                    , Batch_ []
                    )
                )
                model

        GetQnaSessionResponse qnaSessionId result ->
            ( case model.remoteData of
                LoadingQnaSession qnaSessionId_ ->
                    if qnaSessionId == qnaSessionId_ then
                        { model
                            | remoteData =
                                case result of
                                    Ok { isHost, qnaSession } ->
                                        InQnaSession (Types.initInQnaSession qnaSessionId qnaSession isHost)

                                    Err () ->
                                        LoadingQnaSessionFailed ()
                        }

                    else
                        model

                _ ->
                    model
            , Batch_ []
            )

        CreateQnaSessionResponse qnaSessionId hostSecret ->
            case model.remoteData of
                CreatingQnaSession qnaSessionName ->
                    ( { model
                        | remoteData =
                            InQnaSession
                                (Types.initInQnaSession
                                    qnaSessionId
                                    (Moment.init qnaSessionName)
                                    (Just hostSecret)
                                )
                      }
                    , PushUrl model.key (urlEncoder qnaSessionId)
                    )

                _ ->
                    ( model, Batch_ [] )

        GetQnaSessionWithHostInviteResponse hostSecret result ->
            case model.remoteData of
                LoadingQnaSessionWithHostInvite hostSecret_ ->
                    if hostSecret == hostSecret_ then
                        case result of
                            Ok ( qnaSessionId, qnaSession ) ->
                                ( { model
                                    | remoteData =
                                        Types.initInQnaSession qnaSessionId qnaSession (Just hostSecret_)
                                            |> InQnaSession
                                  }
                                , ReplaceUrl model.key (urlEncoder qnaSessionId)
                                )

                            Err () ->
                                ( { model | remoteData = LoadingQnaSessionFailed () }, Batch_ [] )

                    else
                        ( model, Batch_ [] )

                _ ->
                    ( model, Batch_ [] )

        CheckIfConnectedResponse ->
            ( { model | lastConnectionCheck = model.currentTime }
            , Batch_ []
            )

        NewConnection ->
            if model.gotFirstConnectMsg then
                case model.remoteData of
                    Homepage ->
                        homepageRouteInit True model.key

                    LoadingQnaSession qnaSessionId ->
                        qnaSessionRouteInit True model.key qnaSessionId

                    LoadingQnaSessionWithHostInvite hostSecret ->
                        hostInviteRouteInit True model.key hostSecret

                    CreatingQnaSession _ ->
                        homepageRouteInit True model.key

                    LoadingQnaSessionFailed () ->
                        homepageRouteInit True model.key

                    InQnaSession inQnaSession_ ->
                        qnaSessionRouteInit True model.key inQnaSession_.qnaSessionId

            else
                ( { model | gotFirstConnectMsg = True }, Batch_ [] )


css =
    """

@keyframes block-fall {
    0% {
        transform: translateY(-1000px);
    }
    80% {
        transform: translateY(0px);
    }
    90% {
        transform: translateY(-10px);
    }
    100% {
        transform: translateY(0);
    }
}

@keyframes cloud-drift {
    0% {
        transform: translateX(0px);
    }
    50% {
        transform: translateX(2000px);
    }
    50.001% {
        transform: translateX(-2000px);
    }
    100% {
        transform: translateY(0);
    }
}

.pixel-art {
  image-rendering: pixelated;
  image-rendering: -moz-crisp-edges;
  image-rendering: crisp-edges;
  pointer-event: none;
  user-select: none;
}

@keyframes y-offset-adjust-even {
    0% {
        transform: translateY(-100px);
    }
    100% {
        transform: translateY(0);
    }
}

@keyframes y-offset-adjust-odd {
    0% {
        transform: translateY(-100px);
    }
    100% {
        transform: translateY(0);
    }
}

"""


isMobile : ( Quantity Int Pixels, Quantity Int Pixels ) -> Bool
isMobile ( width, _ ) =
    width |> Quantity.lessThan (Pixels.pixels 600)


view : FrontendModel -> { title : String, body : List (Html FrontendMsg) }
view model =
    let
        networkModel : NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg MomentSession
        networkModel =
            case model.remoteData of
                InQnaSession inQnaSession ->
                    inQnaSession.networkModel

                _ ->
                    Network.init
                        { questions = Dict.empty
                        , name = NonemptyString ' ' ""
                        , userId = UserId 0
                        }
    in
    { title = "Moment of the Month"
    , body =
        [ Element.layout
            [ Element.inFront (notConnectedView model)
            , Html.node "style" [] [ Html.text css ] |> Element.html |> Element.behindContent
            , Element.Background.color (Element.rgb 0.95 0.95 1)
            , Element.inFront <|
                Element.el [ Element.width Element.fill, Element.height Element.fill, Element.paddingXY 4 0 ]
                    (case model.remoteData of
                        InQnaSession inQnaSession ->
                            case inQnaSession.isHost of
                                Just _ ->
                                    if Dict.isEmpty (Network.localState qnaSessionUpdate networkModel).questions then
                                        hostView inQnaSession.copiedUrl inQnaSession.qnaSessionId

                                    else
                                        Element.none

                                Nothing ->
                                    questionInputView inQnaSession

                        Homepage ->
                            Element.column
                                (Element.centerY
                                    :: Element.spacing 16
                                    :: Element.paddingXY 16 0
                                    :: Element.width (Element.maximum 800 Element.shrink)
                                    :: inputBackground
                                )
                                [ Element.paragraph
                                    [ Element.centerX, Element.Font.color (Element.rgb 1 1 1) ]
                                    [ Element.text "To join a session, please use the link your host has provided." ]
                                , Element.el
                                    [ Element.Font.size 24
                                    , Element.centerX
                                    , Element.Font.color <| Element.rgb 1 1 1
                                    ]
                                    (Element.text "OR")
                                , Element.Input.button
                                    (Element.centerX :: buttonStyle)
                                    { onPress = Just PressedCreateQnaSession
                                    , label = Element.paragraph [] [ Element.text "Create a new session" ]
                                    }
                                ]

                        LoadingQnaSession _ ->
                            Element.el
                                (Element.centerY :: Element.Font.color (Element.rgb 1 1 1) :: Element.Font.center :: inputBackground)
                                (Element.text "Loading...")

                        LoadingQnaSessionWithHostInvite _ ->
                            Element.el
                                (Element.centerY :: Element.Font.color (Element.rgb 1 1 1) :: Element.Font.center :: inputBackground)
                                (Element.text "Loading...")

                        CreatingQnaSession _ ->
                            Element.el
                                (Element.centerY :: Element.Font.color (Element.rgb 1 1 1) :: Element.Font.center :: inputBackground)
                                (Element.text "Creating...")

                        LoadingQnaSessionFailed () ->
                            Element.el
                                (Element.centerY :: Element.Font.color (Element.rgb 1 1 1) :: Element.Font.center :: inputBackground)
                                (Element.text "Sorry, this session doesn't exist.")
                    )
            ]
            (Element.Lazy.lazy2 questionsView model.windowSize networkModel)
        ]
    }


notConnectedView : FrontendModel -> Element msg
notConnectedView model =
    case ( model.lastConnectionCheck, model.currentTime ) of
        ( Just lastCheck, Just currentTime ) ->
            if
                Duration.from lastCheck currentTime
                    |> Quantity.lessThan (Duration.seconds 30)
            then
                Element.none

            else
                Element.paragraph
                    [ Element.width Element.fill
                    , Element.Background.color <| Element.rgb 1 0.6 0.6
                    , Element.padding 16
                    , Element.Font.center
                    ]
                    [ Element.text "I can't reach the server! Try refreshing the page?" ]

        _ ->
            Element.none


hostView : Maybe Time.Posix -> CryptographicKey QnaSessionId -> Element FrontendMsg
hostView maybeCopiedUrl sessionId =
    Element.column
        (Element.width (Element.maximum 580 Element.fill)
            :: Element.spacing 12
            :: Element.centerY
            :: Element.Font.color (Element.rgb 1 1 1)
            :: inputBackground
        )
        [ Element.el [ Element.centerX, Element.Font.size 36 ] (Element.text "You are the host!")
        , Element.column
            [ Element.width Element.fill, Element.spacing 8 ]
            [ Element.paragraph
                [ Element.Font.center, Element.Font.size 20 ]
                [ Element.text "Copy the link so people can join" ]
            , Element.Input.button
                [ Element.centerX
                , Element.Font.size 20
                , Element.onRight <|
                    case maybeCopiedUrl of
                        Just copiedUrl ->
                            Element.Keyed.el
                                []
                                ( Time.posixToMillis copiedUrl |> String.fromInt
                                , animatedEl
                                    (Animation.steps
                                        { options = [], startAt = [ Property.opacity 0 ] }
                                        [ Animation.step 100 [ Property.opacity 1 ]
                                        , Animation.step 1000 [ Property.opacity 1 ]
                                        , Animation.step 3000 [ Property.opacity 0 ]
                                        ]
                                    )
                                    [ Element.paddingEach { left = 4, right = 0, top = 0, bottom = 0 } ]
                                    (Element.text "Copied!")
                                )

                        Nothing ->
                            Element.none
                ]
                { onPress = Just PressedCopyUrl
                , label =
                    Element.row
                        [ Element.spacing 2 ]
                        [ Element.text (domain ++ urlEncoder sessionId), Element.text "????" ]
                }
            ]
        ]


smallFont : Element.Attr decorative msg
smallFont =
    Element.Font.size 16


copyHostUrlButton : Maybe Time.Posix -> Element FrontendMsg
copyHostUrlButton copiedHostUrl =
    Element.row
        [ Element.width Element.fill, Element.spacing 12, smallFont ]
        [ Element.Input.button
            (buttonStyle ++ [ Element.padding 12 ])
            { onPress = Just PressedCopyHostUrl
            , label = Element.text "Add another host"
            }
        , case copiedHostUrl of
            Just copiedTime ->
                Element.Keyed.el
                    [ Element.width Element.fill ]
                    ( Time.posixToMillis copiedTime |> String.fromInt
                    , animatedParagraph
                        (Animation.steps
                            { options = [], startAt = [ Property.opacity 0 ] }
                            [ Animation.step 100 [ Property.opacity 1 ]
                            , Animation.step 20000 [ Property.opacity 1 ]
                            , Animation.step 3000 [ Property.opacity 0 ]
                            ]
                        )
                        []
                        [ Element.text "Host invite link copied. Paste it to someone so they can join as a host." ]
                    )

            Nothing ->
                Element.none
        ]


maxQuestionChars : number
maxQuestionChars =
    200


inputBackground : List (Element.Attribute msg)
inputBackground =
    [ Element.paddingEach { left = 16, right = 16, top = 20, bottom = 16 }
    , Element.Background.color <| Element.rgba 0 0 0 0.7
    , Element.Border.rounded 8
    , Element.centerX
    ]


questionInputView : InQnaSession_ -> Element FrontendMsg
questionInputView inQnaSession =
    Element.column
        (Element.spacing 16
            :: Element.alignBottom
            :: Element.moveUp 8
            :: Element.width (Element.maximum 800 Element.fill)
            :: inputBackground
        )
        [ Element.el
            [ Element.inFront <|
                Element.el
                    [ Element.alignBottom
                    , Element.alignRight
                    , Element.Font.color <|
                        if String.length inQnaSession.question > maxQuestionChars then
                            errorColor

                        else
                            Element.rgb 0.2 0.2 0.2
                    , Element.Font.size 18
                    , Element.moveLeft 24
                    , Element.moveUp 4
                    ]
                    (Element.text
                        (String.fromInt (String.length inQnaSession.question)
                            ++ "/"
                            ++ String.fromInt maxQuestionChars
                        )
                    )
            , Element.width Element.fill
            ]
            (Element.Input.multiline
                [ Element.height <| Element.px 100
                , Element.htmlAttribute <|
                    Html.Events.preventDefaultOn "keydown"
                        (Json.Decode.map3 (\key shift ctrl -> ( key, shift, ctrl ))
                            (Json.Decode.field "key" Json.Decode.string)
                            (Json.Decode.field "shiftKey" Json.Decode.bool)
                            (Json.Decode.field "ctrlKey" Json.Decode.bool)
                            |> Json.Decode.andThen
                                (\( key, shift, ctrl ) ->
                                    if key == "Enter" && not shift && not ctrl then
                                        Json.Decode.succeed ( PressedCreateQuestion, True )

                                    else
                                        Json.Decode.fail ""
                                )
                        )
                , Element.htmlAttribute <| Html.Attributes.id questionInputId
                ]
                { onChange = TypedQuestion
                , placeholder = Nothing
                , spellcheck = True
                , label =
                    Element.Input.labelAbove
                        [ Element.Font.color <| Element.rgb 1 1 1 ]
                        (Element.paragraph [] [ Element.text "What was your favorite moment this\u{00A0}month?" ])
                , text = inQnaSession.question
                }
            )
        , Element.row [ Element.spacing 16 ]
            [ Element.Input.button
                buttonStyle
                { onPress = Just PressedCreateQuestion
                , label =
                    Element.text "Submit"
                }
            , case ( valiatedQuestion inQnaSession.question, inQnaSession.pressedCreateQuestion ) of
                ( Err error, True ) ->
                    Element.paragraph
                        [ Element.Font.color errorColor ]
                        [ Element.text error ]

                _ ->
                    Element.none
            ]
        ]


valiatedQuestion : String -> Result String NonemptyString
valiatedQuestion text =
    if String.length text > maxQuestionChars then
        Err "Your question is too long"

    else
        case NonemptyString.fromString (String.trim text) of
            Just nonempty ->
                Ok nonempty

            Nothing ->
                Err "Write something first!"


errorColor : Element.Color
errorColor =
    Element.rgb 1 0.5 0.5


topPadding : Quantity Int Pixels
topPadding =
    Pixels.pixels 400


bottomPadding : Quantity number Pixels
bottomPadding =
    Pixels.pixels 260


questionsView :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg MomentSession
    -> Element FrontendMsg
questionsView ( windowWidth, windowHeight ) networkModel =
    let
        isMobile_ =
            isMobile ( windowWidth, windowHeight )

        momentSession =
            Network.localState qnaSessionUpdate networkModel

        currentRow_ =
            Moment.currentRow momentSession

        yOffset_ =
            yOffset currentRow_

        yOffset : Int -> Quantity Int Pixels
        yOffset currentRow =
            Quantity.plus topPadding (towerHeight currentRow)

        towerHeight : Int -> Quantity Int Pixels
        towerHeight currentRow =
            Quantity.multiplyBy currentRow (Moment.momentHeight isMobile_)
                |> Quantity.max
                    (windowHeight
                        |> Quantity.minus topPadding
                        |> Quantity.minus bottomPadding
                        |> Quantity.plus Pixels.pixel
                    )

        towerTallerThanWindowHeight =
            Quantity.multiplyBy currentRow_ (Moment.momentHeight isMobile_)
                |> Quantity.lessThan
                    (windowHeight
                        |> Quantity.minus topPadding
                        |> Quantity.minus bottomPadding
                        |> Quantity.plus Pixels.pixel
                    )

        height : Int -> Quantity Int Pixels
        height currentRow =
            Quantity.plus (yOffset currentRow) bottomPadding

        ground : Element msg
        ground =
            Element.el
                [ Element.width Element.fill
                , Element.height <| pixelLength (Quantity.plus bottomPadding (Moment.momentHeight isMobile_))
                , Element.moveDown <| toFloat <| Pixels.inPixels (Moment.momentHeight isMobile_)
                , Element.Background.gradient
                    { angle = pi
                    , steps =
                        [ Element.rgb 0.8 0.6 0.2
                        , Element.rgb 0.7 0.52 0.15
                        , Element.rgb 0.4 0.3 0.2
                        ]
                    }
                , Element.alignBottom
                ]
                Element.none
    in
    Element.el
        [ Element.width Element.fill
        , Element.height <| pixelLength <| height currentRow_
        , if towerTallerThanWindowHeight then
            Element.htmlAttribute (Html.Attributes.style "animation-name" "")

          else if modBy 2 currentRow_ == 0 then
            Element.htmlAttribute (Html.Attributes.style "animation-name" "y-offset-adjust-even")

          else
            Element.htmlAttribute (Html.Attributes.style "animation-name" "y-offset-adjust-odd")
        , Element.htmlAttribute (Html.Attributes.style "animation-timing-function" "linear")
        , Element.htmlAttribute (Html.Attributes.style "animation-duration" "1s")
        , Element.behindContent (Doodads.mountain yOffset_)
        , Element.behindContent ground
        , Element.behindContent (Doodads.grass yOffset_)
        , Element.behindContent (Doodads.tree yOffset_)
        , Element.behindContent (Doodads.treasure yOffset_)
        , Element.behindContent (Doodads.skeleton yOffset_)
        , Element.behindContent (Doodads.butterflies yOffset_)
        , Element.behindContent (Doodads.flowers yOffset_)
        , Element.behindContent (Doodads.flowers2 yOffset_)
        , Element.behindContent Doodads.sun
        , Element.behindContent (Doodads.cloud1 yOffset_)
        , Element.behindContent (Doodads.cloud2 yOffset_)
        , Element.behindContent (Doodads.cloud3 yOffset_)
        , Element.inFront
            (Element.el
                [ yOffset_
                    |> Quantity.minus (Quantity.multiplyBy currentRow_ (Moment.momentHeight isMobile_))
                    |> Pixels.inPixels
                    |> toFloat
                    |> (+) -10
                    |> Element.moveDown
                , Element.centerX
                , Element.moveLeft (78 + toFloat Moment.maxColumn * 100 / 2)
                ]
                (Element.text
                    (if currentRow_ > 0 then
                        String.fromInt (currentRow_ * 5) ++ " meters ???"

                     else
                        ""
                    )
                )
            )
        , Element.clip
        , Element.inFront
            (Element.el
                (Element.centerX
                    :: Element.width (Element.px (Moment.maxColumn * blockWidth isMobile_))
                    :: List.map
                        (\moment -> questionView isMobile_ (yOffset currentRow_) moment |> Element.inFront)
                        (Dict.values momentSession.questions |> List.reverse)
                )
                Element.none
            )
        ]
        Element.none


blockWidth : Bool -> number
blockWidth isMobile_ =
    if isMobile_ then
        60

    else
        100


questionView : Bool -> Quantity Int Pixels -> Moment -> Element FrontendMsg
questionView isMobile_ offsetY moment =
    let
        width =
            Moment.momentWidth moment

        rightOffset =
            if Moment.momentRow moment |> modBy 2 |> (==) 0 then
                Moment.maxColumn - (Moment.momentColumn moment + width)

            else
                Moment.momentColumn moment
    in
    Element.el
        [ Element.moveRight <| toFloat <| rightOffset * blockWidth isMobile_
        , Quantity.multiplyBy (Moment.momentRow moment + 1) (Quantity.negate (Moment.momentHeight isMobile_))
            |> Quantity.plus offsetY
            |> Pixels.inPixels
            |> toFloat
            |> Element.moveDown
        ]
        (Element.el
            [ Element.width <| Element.px (width * blockWidth isMobile_)
            , Element.height <| pixelLength (Moment.momentHeight isMobile_)
            , Element.Background.color (Moment.momentColor moment)
            , if isMobile_ then
                Element.paddingXY 4 0

              else
                Element.paddingXY 1 0
            , if isMobile_ then
                Element.Border.width 1

              else
                Element.Border.width 2
            , if isMobile_ then
                Element.Border.rounded 2

              else
                Element.Border.rounded 4
            , Element.Border.color (Element.rgb 0.5 0.5 0.5)
            , Element.htmlAttribute (Html.Attributes.style "animation-name" "block-fall")
            , Element.htmlAttribute (Html.Attributes.style "animation-timing-function" "linear")
            , Element.htmlAttribute (Html.Attributes.style "animation-duration" "1s")
            , Element.Font.center
            , Element.Font.size <| Moment.fontSize isMobile_ moment
            ]
            (Element.paragraph
                [ Element.centerY ]
                [ NonemptyString.toString (Moment.momentContent moment) |> Element.text ]
            )
        )


animatedUi : (List (Element.Attribute msg) -> children -> Element msg) -> Animation -> List (Element.Attribute msg) -> children -> Element msg
animatedUi =
    Animated.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }


animatedParagraph : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
animatedParagraph =
    animatedUi Element.paragraph


animatedRow : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
animatedRow =
    animatedUi Element.row


animatedEl : Animation -> List (Element.Attribute msg) -> Element msg -> Element msg
animatedEl =
    animatedUi Element.el


pixelLength : Quantity Int Pixels -> Element.Length
pixelLength =
    Pixels.inPixels >> Element.px


buttonStyle : List (Element.Attr () msg)
buttonStyle =
    [ Element.Background.color <| Element.rgb 0.8 0.8 0.8
    , Element.padding 16
    , Element.Border.rounded 4
    ]
