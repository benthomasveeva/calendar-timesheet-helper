port module Main exposing (..)

import Color
import Color.Convert
import Date exposing (Date)
import Date.Extra
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element, alignLeft, fill, padding, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as JD
import Json.Decode.Pipeline as JDPipe
import Json.Encode as JE
import RemoteData exposing (RemoteData(..))
import Return
import Set exposing (Set)
import Task
import Time


---- MODEL ----


type alias MyData a =
    RemoteData String a


type alias GapiData a =
    RemoteData GapiError a


type GapiError
    = GapiAuthError
    | GapiUnknownError String


type alias YoloData a =
    RemoteData YoloError a


type YoloError
    = NoCredentialsAvailable
    | UnknownYoloError JE.Value


type alias Credential =
    { email : String
    , idToken : String
    }


type alias Event =
    { name : String
    , startDate : Date
    , endDate : Date
    , id : String
    , colorId : String
    , minutes : Int
    }


type alias EventColor =
    { background : String
    , foreground : String
    }


type alias Model =
    { credential : YoloData Credential
    , calendar : MyData String
    , today : MyData Date
    , colors : Dict String EventColor
    , events : GapiData (List Event)
    , updatingEvents : Set String
    , weeksBack : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { credential = NotAsked
      , calendar = NotAsked
      , today = NotAsked
      , colors = Dict.empty
      , events = NotAsked
      , updatingEvents = Set.empty
      , weeksBack = 0
      }
    , Task.perform RxDate Date.now
    )



---- UPDATE ----


completeColorId : String
completeColorId =
    "3"


incompleteColorId : String
incompleteColorId =
    "11"


type Msg
    = RxJsMsg (Result String JsMsg)
    | RxDate Date
    | RefreshEvents
    | NewEvent String
    | MarkComplete String
    | MarkIncomplete String
    | GoBackOneWeek
    | GoForwardOneWeek
    | GoToCurrentWeek


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RxJsMsg jsMsg ->
            case jsMsg of
                Ok GoogleYoloReady ->
                    ( { model | credential = Loading }, sendMsg Login )

                Ok (CredentialResponse response) ->
                    ( { model | credential = response }, respondToCredential response )

                Ok GapiReady ->
                    ( { model | calendar = Loading }, Cmd.batch [ sendMsg LoadCalendars, sendMsg LoadColors ] )

                Ok (CalendarList result) ->
                    let
                        calendar =
                            RemoteData.andThen
                                (List.filter Tuple.second
                                    >> List.head
                                    >> Maybe.map Tuple.first
                                    >> Result.fromMaybe "No Primary Calendar"
                                    >> RemoteData.fromResult
                                )
                                result
                    in
                    Return.singleton
                        { model | calendar = calendar }
                        |> Return.effect_ loadEvents

                Ok (Colors colorsResult) ->
                    Return.singleton { model | colors = colorsResult |> RemoteData.withDefault model.colors }

                Ok (Events eventsResult) ->
                    Return.singleton { model | events = eventsResult }
                        |> Return.command (respondToGapiFailure eventsResult)

                Ok EventCreated ->
                    Return.singleton model
                        |> Return.effect_ loadEvents

                Ok (EventUpdated eventId) ->
                    Return.singleton { model | updatingEvents = Set.remove eventId model.updatingEvents }
                        |> Return.effect_ loadEventsIfNoneUpdating

                Err err ->
                    ( model, Debug.log "jsMsg Error" err |> always Cmd.none )

        RxDate today ->
            ( { model | today = Success today }, Cmd.none )

        RefreshEvents ->
            Return.singleton model
                |> Return.effect_ loadEvents

        NewEvent eventName ->
            Return.singleton model
                |> Return.effect_ (createEvent eventName)

        MarkComplete eventName ->
            Return.singleton model
                |> Return.andThen (changeEventsColor eventName completeColorId)

        MarkIncomplete eventName ->
            Return.singleton model
                |> Return.andThen (changeEventsColor eventName incompleteColorId)

        GoBackOneWeek ->
            Return.singleton { model | weeksBack = model.weeksBack + 1 }
                |> Return.effect_ loadEvents

        GoForwardOneWeek ->
            Return.singleton { model | weeksBack = model.weeksBack - 1 }
                |> Return.effect_ loadEvents

        GoToCurrentWeek ->
            Return.singleton { model | weeksBack = 0 }
                |> Return.effect_ loadEvents


respondToCredential : YoloData Credential -> Cmd msg
respondToCredential loadableCred =
    case loadableCred of
        Success cred ->
            LoadApi cred.email |> sendMsg

        Failure NoCredentialsAvailable ->
            LoginHint |> sendMsg

        Failure (UnknownYoloError _) ->
            Cmd.none

        NotAsked ->
            Cmd.none

        Loading ->
            Cmd.none


respondToGapiFailure : GapiData a -> Cmd msg
respondToGapiFailure loadable =
    case loadable of
        Failure GapiAuthError ->
            sendMsg Login

        Failure (GapiUnknownError error) ->
            Debug.log "Gapi Error" error |> always Cmd.none

        Success _ ->
            Cmd.none

        Loading ->
            Cmd.none

        NotAsked ->
            Cmd.none



---- VIEW ----


viewMyData : (a -> Element msg) -> MyData a -> Element msg
viewMyData viewFun data =
    case data of
        NotAsked ->
            Element.text "-"

        Loading ->
            Element.text "..."

        Failure err ->
            Element.text err

        Success thing ->
            viewFun thing


viewGapiData : (a -> Element msg) -> GapiData a -> Element msg
viewGapiData viewFun =
    RemoteData.mapError toString >> viewMyData viewFun


secondaryButton : Color.Color -> List (Element.Attribute msg) -> List (Element.Attribute msg)
secondaryButton color =
    (++)
        [ Font.color color
        , Element.paddingXY halfGutter quarterGutter
        , Background.color Color.white
        , Border.rounded quarterGutter
        , Border.shadow
            { offset = ( 1, 1 )
            , blur = 1
            , size = 1
            , color = Color.rgba 0 0 0 0.15
            }
        ]


gutter : number
gutter =
    20


halfGutter : number
halfGutter =
    10


quarterGutter : number
quarterGutter =
    5


view : Model -> Html Msg
view model =
    Element.layout [ Background.color Color.white, Font.color Color.charcoal, Font.size 16 ] <|
        Element.column [ spacing halfGutter, alignLeft ]
            [ viewHeaderRow model
            , viewBody model
            ]


viewHeaderRow : Model -> Element Msg
viewHeaderRow model =
    Element.row [ width fill, alignLeft, padding gutter, Background.color Color.lightGray, spacing halfGutter ]
        [ Element.text "Calendar Timesheet Helper"
        , Input.button (secondaryButton Color.blue [ Element.alignRight ])
            { onPress = Just RefreshEvents
            , label = Element.text "Refresh Events"
            }
        , Element.el [ Element.alignRight ] <|
            viewMyData Element.text model.calendar
        ]


viewBody : Model -> Element Msg
viewBody model =
    Element.column [ Element.paddingXY gutter halfGutter, spacing halfGutter ]
        [ viewInputRow model
        , viewGapiData (viewWeekTable model.colors) model.events
        ]


viewInputRow : Model -> Element Msg
viewInputRow model =
    Element.row [ Background.color Color.lightGray, Border.rounded quarterGutter, padding halfGutter, spacing halfGutter ]
        [ Input.button (secondaryButton Color.blue [ alignLeft ])
            { onPress = Just GoBackOneWeek
            , label = Element.text "<"
            }
        , Element.text <| "Week starting " ++ RemoteData.withDefault "?" (RemoteData.map (Date.Extra.toFormattedString "MM-dd-yyyy" << Date.Extra.floor Date.Extra.Monday << Date.Extra.add Date.Extra.Week (-1 * model.weeksBack)) model.today)
        , Input.button (secondaryButton Color.blue [ alignLeft ])
            { onPress = Just GoToCurrentWeek
            , label = Element.text "Current Week"
            }
        , Input.button (secondaryButton Color.blue [ alignLeft ])
            { onPress = Just GoForwardOneWeek
            , label = Element.text ">"
            }
        ]


type alias ColorId =
    String


type alias TableData =
    { client : String
    , monday : Dict ColorId Int
    , tuesday : Dict ColorId Int
    , wednesday : Dict ColorId Int
    , thursday : Dict ColorId Int
    , friday : Dict ColorId Int
    , isTotal : Bool
    }


viewWeekTable : Dict ColorId EventColor -> List Event -> Element Msg
viewWeekTable colorDict events =
    Element.table [ spacing halfGutter ]
        { data = makeTableData events
        , columns =
            [ { header = Element.empty
              , view = .client >> Element.text >> Element.el [ Element.centerY ]
              }
            , { header = Element.text "Monday"
              , view = \data -> data.monday |> viewColorBreakdown colorDict |> highlightIfTotal data
              }
            , { header = Element.text "Tuesday"
              , view = \data -> data.tuesday |> viewColorBreakdown colorDict |> highlightIfTotal data
              }
            , { header = Element.text "Wednesday"
              , view = \data -> data.wednesday |> viewColorBreakdown colorDict |> highlightIfTotal data
              }
            , { header = Element.text "Thursday"
              , view = \data -> data.thursday |> viewColorBreakdown colorDict |> highlightIfTotal data
              }
            , { header = Element.text "Friday"
              , view = \data -> data.friday |> viewColorBreakdown colorDict |> highlightIfTotal data
              }
            , { header = Element.text "Total"
              , view = \data -> data |> getWeeklyTotal |> viewColorBreakdown colorDict |> Element.el [ Font.color Color.lightCharcoal ]
              }
            , { header = Element.empty
              , view = viewNewInstanceButton
              }
            , { header = Element.empty
              , view = viewMarkCompleteButton
              }
            , { header = Element.empty
              , view = viewMarkIncompleteButton
              }
            ]
        }


makeTableData : List Event -> List TableData
makeTableData events =
    List.foldl addEventToTableDataDict Dict.empty events
        |> Dict.values
        |> flip (++) [ List.foldl addEventToTableData (defaultTableData "Total") events |> (\data -> { data | isTotal = True }) ]


addEventToTableDataDict : Event -> Dict String TableData -> Dict String TableData
addEventToTableDataDict event dict =
    Dict.update event.name (Maybe.withDefault (defaultTableData event.name) >> addEventToTableData event >> Just) dict


defaultTableData : String -> TableData
defaultTableData name =
    { client = name
    , monday = Dict.empty
    , tuesday = Dict.empty
    , wednesday = Dict.empty
    , thursday = Dict.empty
    , friday = Dict.empty
    , isTotal = False
    }


addEventToTableData : Event -> TableData -> TableData
addEventToTableData event data =
    case event.startDate |> Date.Extra.weekdayNumber |> Date.Extra.numberToWeekday of
        Date.Mon ->
            { data | monday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.monday }

        Date.Tue ->
            { data | tuesday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.tuesday }

        Date.Wed ->
            { data | wednesday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.wednesday }

        Date.Thu ->
            { data | thursday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.thursday }

        Date.Fri ->
            { data | friday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.friday }

        Date.Sat ->
            { data | monday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.monday }

        Date.Sun ->
            { data | monday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.monday }


getWeeklyTotal : TableData -> Dict ColorId Int
getWeeklyTotal data =
    data.monday
        |> mergeColorBreakdown data.tuesday
        |> mergeColorBreakdown data.wednesday
        |> mergeColorBreakdown data.thursday
        |> mergeColorBreakdown data.friday


mergeColorBreakdown : Dict ColorId Int -> Dict ColorId Int -> Dict ColorId Int
mergeColorBreakdown left right =
    Dict.toList left ++ Dict.toList right |> Dict.Extra.fromListDedupe (+)


highlightIfTotal : TableData -> Element msg -> Element msg
highlightIfTotal data =
    if data.isTotal then
        Element.el [ Font.color Color.lightCharcoal ]
    else
        identity


viewColorBreakdown : Dict ColorId EventColor -> Dict ColorId Int -> Element msg
viewColorBreakdown colorDict minutesByColor =
    minutesByColor
        |> Dict.toList
        |> List.map
            (\( colorId, minutes ) ->
                Element.row [ spacing quarterGutter, Element.centerX, Element.centerY ]
                    [ Dict.get colorId colorDict
                        |> Maybe.andThen (.background >> Color.Convert.hexToColor >> Result.toMaybe)
                        |> Maybe.map
                            (\color ->
                                Element.el [ Element.centerY, width <| Element.px halfGutter, Element.height <| Element.px halfGutter, Background.color color ] Element.empty
                            )
                        |> Maybe.withDefault (Element.text <| colorId ++ ":")
                    , Element.text <| toString (toFloat minutes / 60)
                    ]
            )
        |> Element.row [ spacing halfGutter, width fill, Element.centerY ]


viewNewInstanceButton : TableData -> Element Msg
viewNewInstanceButton data =
    case data.isTotal of
        True ->
            Element.empty

        False ->
            Input.button (secondaryButton Color.darkGreen [])
                { onPress = Just <| NewEvent data.client
                , label = Element.text "New Instance"
                }


viewMarkCompleteButton : TableData -> Element Msg
viewMarkCompleteButton data =
    case data.isTotal of
        True ->
            Element.empty

        False ->
            Input.button (secondaryButton Color.blue [])
                { onPress = Just <| MarkComplete data.client
                , label = Element.text "Complete"
                }


viewMarkIncompleteButton : TableData -> Element Msg
viewMarkIncompleteButton data =
    case data.isTotal of
        True ->
            Element.empty

        False ->
            Input.button (secondaryButton Color.blue [])
                { onPress = Just <| MarkIncomplete data.client
                , label = Element.text "Incomplete"
                }



---- COMMANDS ----


type OutMsg
    = Login
    | LoginHint
    | LoadApi String
    | LoadCalendars
    | LoadColors
    | LoadEvents String String String
    | CreateEvent String String String String
    | ChangeEventColor String String ColorId


port elmToJs : JE.Value -> Cmd msg


sendMsg : OutMsg -> Cmd msg
sendMsg =
    encodeOutMsg >> elmToJs


encodeOutMsg : OutMsg -> JE.Value
encodeOutMsg msg =
    case msg of
        Login ->
            JE.object [ ( "msg", JE.string "login" ) ]

        LoginHint ->
            JE.object [ ( "msg", JE.string "loginHint" ) ]

        LoadApi email ->
            JE.object
                [ ( "msg", JE.string "loadapi" )
                , ( "email", JE.string email )
                ]

        LoadCalendars ->
            JE.object [ ( "msg", JE.string "loadCalendars" ) ]

        LoadColors ->
            JE.object [ ( "msg", JE.string "loadColors" ) ]

        LoadEvents calendarId startDateString endDateString ->
            JE.object
                [ ( "msg", JE.string "loadEvents" )
                , ( "calendar", JE.string calendarId )
                , ( "timeMin", JE.string startDateString )
                , ( "timeMax", JE.string endDateString )
                ]

        CreateEvent calendarId eventName startTimeString endTimeString ->
            JE.object
                [ ( "msg", JE.string "createEvent" )
                , ( "calendar", JE.string calendarId )
                , ( "eventName", JE.string eventName )
                , ( "startTime", JE.string startTimeString )
                , ( "endTime", JE.string endTimeString )
                ]

        ChangeEventColor calendarId eventId colorId ->
            JE.object
                [ ( "msg", JE.string "changeEventColor" )
                , ( "calendar", JE.string calendarId )
                , ( "event", JE.string eventId )
                , ( "color", JE.string colorId )
                ]


loadEvents : Model -> Cmd msg
loadEvents model =
    RemoteData.map2
        (\calendar date ->
            LoadEvents calendar
                (Date.Extra.toIsoString <| Date.Extra.floor Date.Extra.Saturday <| Date.Extra.add Date.Extra.Week (-1 * model.weeksBack) date)
                (Date.Extra.toIsoString <| Date.Extra.ceiling Date.Extra.Saturday <| Date.Extra.add Date.Extra.Week (-1 * model.weeksBack) date)
                |> sendMsg
        )
        model.calendar
        model.today
        |> RemoteData.withDefault Cmd.none


loadEventsIfNoneUpdating : Model -> Cmd Msg
loadEventsIfNoneUpdating model =
    if Set.isEmpty model.updatingEvents then
        loadEvents model
    else
        Cmd.none


createEvent : String -> Model -> Cmd msg
createEvent eventName model =
    RemoteData.map2
        (\calId now ->
            CreateEvent calId
                eventName
                (now |> Date.Extra.floor Date.Extra.Hour |> Date.Extra.add Date.Extra.Hour -1 |> Date.Extra.toIsoString)
                (now |> Date.Extra.floor Date.Extra.Hour |> Date.Extra.toIsoString)
                |> sendMsg
        )
        model.calendar
        model.today
        |> RemoteData.withDefault Cmd.none


changeEventsColor : String -> ColorId -> Model -> ( Model, Cmd msg )
changeEventsColor eventName colorId model =
    let
        eventIds =
            RemoteData.withDefault [] model.events
                |> List.filter (.name >> (==) eventName)
                |> List.map .id

        command =
            RemoteData.map
                (\calId ->
                    eventIds
                        |> List.map (changeEventColor calId colorId)
                        |> Cmd.batch
                )
                model.calendar
                |> RemoteData.withDefault Cmd.none
    in
    ( { model | updatingEvents = Set.union model.updatingEvents (Set.fromList eventIds) }, command )


changeEventColor : String -> ColorId -> String -> Cmd msg
changeEventColor calId colorId eventId =
    ChangeEventColor calId eventId colorId
        |> sendMsg



---- SUBSCRIPTIONS ----


type JsMsg
    = GoogleYoloReady
    | CredentialResponse (YoloData Credential)
    | GapiReady
    | CalendarList (MyData (List ( String, Bool )))
    | Colors (MyData (Dict ColorId EventColor))
    | Events (GapiData (List Event))
    | EventCreated
    | EventUpdated String


port jsToElm : (JE.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ jsToElm (JD.decodeValue jsMsgDecoder >> RxJsMsg)
        , Time.every (5 * Time.second) (Date.fromTime >> RxDate)
        , Time.every (60 * Time.minute) (always RefreshEvents)
        ]


jsMsgDecoder : JD.Decoder JsMsg
jsMsgDecoder =
    pickDecoder "msg"
        JD.string
        [ ( "googleyoloready", JD.succeed GoogleYoloReady )
        , ( "credentials", credentialDecoder )
        , ( "gapiready", JD.succeed GapiReady )
        , ( "calendarlist", calendarListDecoder )
        , ( "colors", colorsDecoder )
        , ( "events", eventsDecoder )
        , ( "eventcreated", JD.succeed EventCreated )
        , ( "eventupdated", JD.map EventUpdated (JD.field "eventId" JD.string) )
        ]


credentialDecoder : JD.Decoder JsMsg
credentialDecoder =
    JD.oneOf
        [ credentialSuccessDecoder, credentialFailDecoder ]


credentialSuccessDecoder : JD.Decoder JsMsg
credentialSuccessDecoder =
    JD.map2 Credential
        (JD.field "email" JD.string)
        (JD.field "idToken" JD.string)
        |> JD.map (Success >> CredentialResponse)


credentialFailDecoder : JD.Decoder JsMsg
credentialFailDecoder =
    let
        handleErrorType errorType =
            case errorType of
                "noCredentialsAvailable" ->
                    JD.succeed NoCredentialsAvailable

                _ ->
                    JD.value |> JD.map UnknownYoloError
    in
    JDPipe.decode handleErrorType
        |> JDPipe.requiredAt [ "error", "type" ] JD.string
        |> JDPipe.resolve
        |> JD.map (Failure >> CredentialResponse)


calendarListDecoder : JD.Decoder JsMsg
calendarListDecoder =
    JD.oneOf
        [ calendarListSuccessDecoder
        , JD.succeed (Failure "Failed to get calendar list. See console.")
        ]
        |> JD.map CalendarList


calendarListSuccessDecoder : JD.Decoder (RemoteData String (List ( String, Bool )))
calendarListSuccessDecoder =
    JD.map Success <|
        JD.field "items" <|
            JD.list
                (JD.map2 (,)
                    (JD.field "id" JD.string)
                    (JD.map (Maybe.withDefault False) <| JD.maybe <| JD.field "primary" JD.bool)
                )


colorsDecoder : JD.Decoder JsMsg
colorsDecoder =
    JD.oneOf
        [ JD.map (Colors << Success) <| JD.field "eventColors" <| JD.dict <| JD.map2 EventColor (JD.field "background" JD.string) (JD.field "foreground" JD.string)
        , JD.map (Colors << Failure) <| JD.map toString <| JD.field "error" JD.value
        ]


eventsDecoder : JD.Decoder JsMsg
eventsDecoder =
    JD.oneOf
        [ JD.map (Events << Success) <| JD.field "events" (JD.map (List.filterMap identity) <| JD.list eventDecoder)
        , JD.map (Events << Failure) <| gapiErrorDecoder
        ]


eventDecoder : JD.Decoder (Maybe Event)
eventDecoder =
    JD.maybe <|
        JD.map5 makeEvent
            (JD.field "summary" JD.string)
            (JD.at [ "start", "dateTime" ] dateDecoder)
            (JD.at [ "end", "dateTime" ] dateDecoder)
            (JD.field "id" JD.string)
            (JD.field "colorId" JD.string)


makeEvent : String -> Date -> Date -> String -> String -> Event
makeEvent name start end id color =
    Event name start end id color (Date.Extra.diff Date.Extra.Minute start end)


dateDecoder : JD.Decoder Date
dateDecoder =
    let
        toDateDecoder string =
            case Date.Extra.fromIsoString string of
                Ok date ->
                    JD.succeed date

                Err err ->
                    JD.fail err
    in
    JD.string |> JD.andThen toDateDecoder


gapiErrorDecoder : JD.Decoder GapiError
gapiErrorDecoder =
    let
        statusToDecoder status =
            case status of
                401 ->
                    JD.succeed GapiAuthError

                _ ->
                    JD.value |> JD.map (toString >> GapiUnknownError)
    in
    JD.at [ "error", "result", "error", "code" ] JD.int |> JD.andThen statusToDecoder


pickDecoder : String -> JD.Decoder comparable -> List ( comparable, JD.Decoder b ) -> JD.Decoder b
pickDecoder fieldName fieldDecoder decoders =
    JD.field fieldName fieldDecoder
        |> JD.andThen
            (\fieldValue ->
                decoders
                    |> List.filter (Tuple.first >> (==) fieldValue)
                    |> List.head
                    |> Maybe.map Tuple.second
                    |> Maybe.withDefault (JD.fail <| "Could not find decoder for field " ++ fieldName ++ " value of " ++ toString fieldValue)
            )



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
