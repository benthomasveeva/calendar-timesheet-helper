module Main exposing (main)

import Browser
import Browser.Navigation
import Color
import Color.Convert
import Date exposing (Date)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element, alignLeft, fill, padding, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Iso8601
import Json.Decode as JD
import Json.Decode.Extra as JDExtra
import Json.Encode as JE
import OAuth
import OAuth.Implicit
import RemoteData exposing (RemoteData(..), WebData)
import Return
import Task exposing (Task)
import Time
import Time.Extra
import Url



---- MODEL ----


type alias Event =
    { name : String
    , startDate : Time.Posix
    , endDate : Time.Posix
    , id : String
    , colorId : String
    , minutes : Int
    }


type alias EventColor =
    { background : String
    , foreground : String
    }


type alias Model =
    { calendar : WebData String
    , now : Maybe Time.Posix
    , timeZone : Maybe Time.Zone
    , colors : Dict String EventColor
    , events : WebData (List Event)
    , weeksBack : Int
    , auth : Maybe OAuth.Implicit.AuthorizationSuccess
    , url : Url.Url
    , key : Browser.Navigation.Key
    , newEventName : String
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    Return.singleton
        { calendar = NotAsked
        , now = Nothing
        , timeZone = Nothing
        , colors = Dict.empty
        , events = NotAsked
        , weeksBack = 0
        , auth = OAuth.Implicit.parseToken url |> authResultToMaybe
        , url = url
        , key = key
        , newEventName = ""
        }
        |> Return.command (Task.perform RxCurrentTime Time.now)
        |> Return.command (Task.perform RxTimeZone Time.here)
        |> Return.effect_ (.auth >> Maybe.map getCalendarList >> Maybe.withDefault Cmd.none)
        |> Return.effect_ (.auth >> Maybe.map getColors >> Maybe.withDefault Cmd.none)
        |> Return.command (Browser.Navigation.replaceUrl key <| "/")


authResultToMaybe : OAuth.Implicit.AuthorizationResult -> Maybe OAuth.Implicit.AuthorizationSuccess
authResultToMaybe result =
    case result of
        OAuth.Implicit.Empty ->
            Nothing

        OAuth.Implicit.Error _ ->
            Nothing

        OAuth.Implicit.Success success ->
            Just success



---- UPDATE ----


completeColorId : String
completeColorId =
    "3"


incompleteColorId : String
incompleteColorId =
    "11"


type Msg
    = RxPrimaryCalendarId (WebData String)
    | RxColors (WebData (Dict ColorId EventColor))
    | RxEvents (WebData (List Event))
    | RxEventAck (WebData String)
    | RxCurrentTime Time.Posix
    | RxTimeZone Time.Zone
    | RefreshEvents
    | NewInstance String
    | CreateNewEvent
    | MarkComplete String
    | MarkIncomplete String
    | GoBackOneWeek
    | GoForwardOneWeek
    | GoToCurrentWeek
    | OAuthAuthorize
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | SetNewEventName String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RxPrimaryCalendarId data ->
            Return.singleton { model | calendar = data }
                |> Return.effect_ loadEvents
                |> Return.effect_ (.url >> handle401 data)

        RxColors data ->
            Return.singleton { model | colors = RemoteData.withDefault model.colors data }
                |> Return.effect_ (.url >> handle401 data)

        RxEvents data ->
            Return.singleton { model | events = data }
                |> Return.effect_ (.url >> handle401 data)

        RxEventAck data ->
            Return.singleton model
                |> Return.effect_ loadEvents
                |> Return.effect_ (.url >> handle401 data)

        RxCurrentTime now ->
            ( { model | now = Just now }, Cmd.none )

        RxTimeZone zone ->
            Return.singleton { model | timeZone = Just zone }

        RefreshEvents ->
            Return.singleton model
                |> Return.effect_ loadEvents

        NewInstance eventName ->
            Return.singleton model
                |> Return.effect_ (createEvent eventName)

        CreateNewEvent ->
            Return.singleton { model | newEventName = "" }
                |> Return.effect_ (createEvent model.newEventName)

        MarkComplete eventName ->
            Return.singleton model
                |> Return.effect_ (changeEventsColor eventName completeColorId)

        MarkIncomplete eventName ->
            Return.singleton model
                |> Return.effect_ (changeEventsColor eventName incompleteColorId)

        GoBackOneWeek ->
            Return.singleton { model | weeksBack = model.weeksBack + 1 }
                |> Return.effect_ loadEvents

        GoForwardOneWeek ->
            Return.singleton { model | weeksBack = model.weeksBack - 1 }
                |> Return.effect_ loadEvents

        GoToCurrentWeek ->
            Return.singleton { model | weeksBack = 0 }
                |> Return.effect_ loadEvents

        OAuthAuthorize ->
            Return.singleton model
                |> Return.effect_ (.url >> login)

        UrlRequested urlRequest ->
            Return.singleton model

        UrlChanged url ->
            Return.singleton { model | url = url }

        SetNewEventName newName ->
            Return.singleton { model | newEventName = newName }



---- COMMANDS ----


login : Url.Url -> Cmd Msg
login url =
    OAuth.Implicit.makeAuthUrl
        { clientId = "159193416244-l4tsfgdhbn402qq57ajahsf3cu41vno0.apps.googleusercontent.com"
        , redirectUri = { url | query = Nothing, fragment = Nothing }
        , scope = [ "email", "profile", "https://www.googleapis.com/auth/calendar" ]
        , state = Nothing
        , url =
            { protocol = Url.Https
            , host = "accounts.google.com"
            , port_ = Nothing
            , path = "/o/oauth2/v2/auth"
            , query = Nothing
            , fragment = Nothing
            }
        }
        |> Url.toString
        |> Browser.Navigation.load


handle401 : WebData a -> Url.Url -> Cmd Msg
handle401 data =
    case data of
        Failure (Http.BadStatus response) ->
            if response.status.code == 401 then
                login

            else
                always Cmd.none

        _ ->
            always Cmd.none


getCalendarList : OAuth.Implicit.AuthorizationSuccess -> Cmd Msg
getCalendarList response =
    let
        calendarListDecoder =
            JD.field "items" <|
                JD.list
                    (JD.map2 (\a b -> ( a, b ))
                        (JD.field "id" JD.string)
                        (JD.map (Maybe.withDefault False) <| JD.maybe <| JD.field "primary" JD.bool)
                    )

        findPrimaryCalendar =
            List.filter Tuple.second
                >> List.head
                >> Maybe.map Tuple.first
                >> Result.fromMaybe "No Primary Calendar Found"
                >> JDExtra.fromResult
    in
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken response.token []
        , withCredentials = False
        , url = "https://www.googleapis.com/calendar/v3/users/me/calendarList"
        , expect = Http.expectJson (calendarListDecoder |> JD.andThen findPrimaryCalendar)
        , timeout = Nothing
        }
        |> RemoteData.sendRequest
        |> Cmd.map RxPrimaryCalendarId


getColors : OAuth.Implicit.AuthorizationSuccess -> Cmd Msg
getColors response =
    let
        decoder =
            JD.field "event" <|
                JD.dict <|
                    JD.map2 EventColor
                        (JD.field "background" JD.string)
                        (JD.field "foreground" JD.string)
    in
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken response.token []
        , withCredentials = False
        , url = "https://www.googleapis.com/calendar/v3/colors"
        , expect = Http.expectJson decoder
        , timeout = Nothing
        }
        |> RemoteData.sendRequest
        |> Cmd.map RxColors


loadEvents : Model -> Cmd Msg
loadEvents model =
    Maybe.map4 (getEventsRequest model.weeksBack)
        model.timeZone
        model.now
        (RemoteData.toMaybe model.calendar)
        model.auth
        |> Maybe.map (RemoteData.sendRequest >> Cmd.map RxEvents)
        |> Maybe.withDefault Cmd.none


getEventsRequest : Int -> Time.Zone -> Time.Posix -> String -> OAuth.Implicit.AuthorizationSuccess -> Http.Request (List Event)
getEventsRequest weeksBack zone now calendarId response =
    let
        decoder =
            JD.field "items" (JD.map (List.filterMap identity) <| JD.list eventDecoder)

        eventDecoder =
            JD.maybe <|
                JD.map5 makeEvent
                    (JD.field "summary" JD.string)
                    (JD.at [ "start", "dateTime" ] Iso8601.decoder)
                    (JD.at [ "end", "dateTime" ] Iso8601.decoder)
                    (JD.field "id" JD.string)
                    (JD.field "colorId" JD.string)

        makeEvent summary start end id colorId =
            Event summary start end id colorId (Time.Extra.diff Time.Extra.Minute zone start end)
    in
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken response.token []
        , withCredentials = False
        , url =
            "https://www.googleapis.com/calendar/v3/calendars/"
                ++ calendarId
                ++ "/events?singleEvents=true&timeMin="
                ++ (Iso8601.fromTime <| Time.Extra.floor Time.Extra.Saturday zone <| Time.Extra.add Time.Extra.Week (-1 * weeksBack) zone now)
                ++ "&timeMax="
                ++ (Iso8601.fromTime <| Time.Extra.ceiling Time.Extra.Saturday zone <| Time.Extra.add Time.Extra.Week (-1 * weeksBack) zone now)
        , expect = Http.expectJson decoder
        , timeout = Nothing
        }


createEvent : String -> Model -> Cmd Msg
createEvent eventName model =
    Maybe.map4 (createEventRequest eventName)
        model.timeZone
        model.now
        (RemoteData.toMaybe model.calendar)
        model.auth
        |> Maybe.map (RemoteData.sendRequest >> Cmd.map RxEventAck)
        |> Maybe.withDefault Cmd.none


createEventRequest : String -> Time.Zone -> Time.Posix -> String -> OAuth.Implicit.AuthorizationSuccess -> Http.Request String
createEventRequest eventName zone now calendarId oauth =
    let
        bodyValue =
            JE.object
                [ ( "colorId", JE.string "11" )
                , ( "summary", JE.string eventName )
                , ( "visibility", JE.string "private" )
                , ( "transparency", JE.string "transparent" )
                , ( "reminders"
                  , JE.object
                        [ ( "useDefault", JE.bool False )
                        , ( "overrides", JE.list identity [] )
                        ]
                  )
                , ( "start", JE.object [ ( "dateTime", JE.string <| Iso8601.fromTime <| Time.Extra.add Time.Extra.Hour -1 zone <| Time.Extra.floor Time.Extra.Hour zone now ) ] )
                , ( "end", JE.object [ ( "dateTime", JE.string <| Iso8601.fromTime <| Time.Extra.floor Time.Extra.Hour zone now ) ] )
                ]
    in
    Http.request
        { method = "POST"
        , body = Http.jsonBody bodyValue
        , headers = OAuth.useToken oauth.token []
        , withCredentials = False
        , url =
            "https://www.googleapis.com/calendar/v3/calendars/"
                ++ calendarId
                ++ "/events"
        , expect = Http.expectString
        , timeout = Nothing
        }


changeEventsColor : String -> ColorId -> Model -> Cmd Msg
changeEventsColor eventName newColor model =
    Maybe.map3 (changeEventsColorHelper eventName newColor)
        (RemoteData.toMaybe model.events)
        (RemoteData.toMaybe model.calendar)
        model.auth
        |> Maybe.andThen
            (\task ->
                Maybe.map4
                    (\zone now calendar oauth ->
                        task
                            |> Task.andThen
                                (\_ ->
                                    getEventsRequest model.weeksBack zone now calendar oauth
                                        |> Http.toTask
                                        |> RemoteData.fromTask
                                )
                    )
                    model.timeZone
                    model.now
                    (RemoteData.toMaybe model.calendar)
                    model.auth
            )
        |> Maybe.map (Task.perform RxEvents)
        |> Maybe.withDefault Cmd.none


changeEventsColorHelper : String -> ColorId -> List Event -> String -> OAuth.Implicit.AuthorizationSuccess -> Task Never (List (WebData String))
changeEventsColorHelper eventName newColor events calendarId oauth =
    events
        |> List.filter (.name >> (==) eventName)
        |> List.map (.id >> changeEventColorRequest calendarId newColor oauth >> Http.toTask >> RemoteData.fromTask)
        |> Task.sequence


changeEventColorRequest : String -> ColorId -> OAuth.Implicit.AuthorizationSuccess -> String -> Http.Request String
changeEventColorRequest calendarId colorId oauth eventId =
    let
        bodyValue =
            JE.object
                [ ( "colorId", JE.string colorId )
                ]
    in
    Http.request
        { method = "PATCH"
        , body = Http.jsonBody bodyValue
        , headers = OAuth.useToken oauth.token []
        , withCredentials = False
        , url =
            "https://www.googleapis.com/calendar/v3/calendars/"
                ++ calendarId
                ++ "/events/"
                ++ eventId
        , expect = Http.expectString
        , timeout = Nothing
        }



---- VIEW ----


viewWebData : (a -> Element msg) -> WebData a -> Element msg
viewWebData viewFun data =
    case data of
        NotAsked ->
            Element.text "-"

        Loading ->
            Element.text "..."

        Failure err ->
            Element.text <| httpErrorToString err

        Success thing ->
            viewFun thing


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl invalidUrl ->
            "Bad URL: " ++ invalidUrl

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus response ->
            "Received Status Code: " ++ String.fromInt response.status.code

        Http.BadPayload errText _ ->
            "Bad Payload: " ++ errText


secondaryButton : Color.Color -> List (Element.Attribute msg) -> List (Element.Attribute msg)
secondaryButton color =
    (++)
        [ Font.color (convertColor color)
        , Element.paddingXY halfGutter quarterGutter
        , Background.color (convertColor Color.white)
        , Border.rounded quarterGutter
        , Border.shadow
            { offset = ( 1, 1 )
            , blur = 1
            , size = 1
            , color = convertColor <| Color.rgba 0 0 0 0.15
            }
        ]


convertColor : Color.Color -> Element.Color
convertColor =
    Color.toRgba
        >> Element.fromRgb


gutter : number
gutter =
    20


halfGutter : number
halfGutter =
    10


quarterGutter : number
quarterGutter =
    5


view : Model -> Browser.Document Msg
view model =
    { title = "Calendar Helper"
    , body =
        [ Element.layout
            [ Background.color <| convertColor Color.white
            , Font.color <| convertColor Color.charcoal
            , Font.size 16
            ]
          <|
            Element.column [ spacing halfGutter, alignLeft, width fill ]
                [ viewHeaderRow model
                , viewBody model
                ]
        ]
    }


viewHeaderRow : Model -> Element Msg
viewHeaderRow model =
    Element.row
        [ width fill
        , alignLeft
        , padding gutter
        , Background.color <| convertColor Color.lightGray
        , spacing halfGutter
        ]
        [ Element.text "Calendar Timesheet Helper"
        , Input.button (secondaryButton Color.blue [ Element.alignRight ])
            { onPress = Just RefreshEvents
            , label = Element.text "Refresh Events"
            }
        , Input.button (secondaryButton Color.green [ Element.alignRight ])
            { onPress = Just OAuthAuthorize
            , label = Element.text "Begin OAuth"
            }
        , Element.el [ Element.alignRight ] <|
            viewWebData Element.text model.calendar
        ]


viewBody : Model -> Element Msg
viewBody model =
    Element.column [ Element.paddingXY gutter halfGutter, spacing halfGutter, width fill ]
        [ viewInputRow model
        , viewWebData (viewWeekTable (Maybe.withDefault Time.utc model.timeZone) model.colors) model.events
        ]


viewInputRow : Model -> Element Msg
viewInputRow model =
    Element.row
        [ Background.color <| convertColor Color.lightGray
        , Border.rounded quarterGutter
        , padding halfGutter
        , spacing halfGutter
        , width fill
        ]
        [ Input.button (secondaryButton Color.blue [ alignLeft ])
            { onPress = Just GoBackOneWeek
            , label = Element.text "<"
            }
        , Element.text <| "Week starting " ++ Maybe.withDefault "?" (Maybe.map2 (weekStartToString model.weeksBack) model.timeZone model.now)
        , Input.button (secondaryButton Color.blue [ alignLeft ])
            { onPress = Just GoToCurrentWeek
            , label = Element.text "Current Week"
            }
        , Input.button (secondaryButton Color.blue [ alignLeft ])
            { onPress = Just GoForwardOneWeek
            , label = Element.text ">"
            }
        , newEventInput model
        ]


newEventInput : Model -> Element Msg
newEventInput model =
    Element.row [ Element.alignRight, spacing halfGutter, width Element.shrink ]
        [ Input.text []
            { onChange = SetNewEventName
            , text = model.newEventName
            , placeholder = Nothing
            , label = Input.labelHidden "New Event"
            }
        , Input.button (secondaryButton Color.darkGreen [])
            { onPress = Just CreateNewEvent
            , label = Element.text "New Event"
            }
        ]


weekStartToString : Int -> Time.Zone -> Time.Posix -> String
weekStartToString weeksBack zone now =
    Time.Extra.add Time.Extra.Week (-1 * weeksBack) zone now
        |> Time.Extra.floor Time.Extra.Monday zone
        |> Date.fromPosix zone
        |> Date.format "MM-dd-yyyy"


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


viewWeekTable : Time.Zone -> Dict ColorId EventColor -> List Event -> Element Msg
viewWeekTable zone colorDict events =
    Element.table [ spacing halfGutter, width Element.shrink ]
        { data = makeTableData zone events
        , columns =
            [ { header = Element.none
              , width = fill
              , view = .client >> Element.text >> Element.el [ Element.centerY ]
              }
            , { header = Element.text "Monday"
              , width = fill
              , view = \data -> data.monday |> viewColorBreakdown colorDict |> highlightIfTotal data
              }
            , { header = Element.text "Tuesday"
              , width = fill
              , view = \data -> data.tuesday |> viewColorBreakdown colorDict |> highlightIfTotal data
              }
            , { header = Element.text "Wednesday"
              , width = fill
              , view = \data -> data.wednesday |> viewColorBreakdown colorDict |> highlightIfTotal data
              }
            , { header = Element.text "Thursday"
              , width = fill
              , view = \data -> data.thursday |> viewColorBreakdown colorDict |> highlightIfTotal data
              }
            , { header = Element.text "Friday"
              , width = fill
              , view = \data -> data.friday |> viewColorBreakdown colorDict |> highlightIfTotal data
              }
            , { header = Element.text "Total"
              , width = fill
              , view = \data -> data |> getWeeklyTotal |> viewColorBreakdown colorDict |> Element.el [ Font.color <| convertColor Color.lightCharcoal ]
              }
            , { header = Element.none
              , width = fill
              , view = viewNewInstanceButton
              }
            , { header = Element.none
              , width = fill
              , view = viewMarkCompleteButton
              }
            , { header = Element.none
              , width = fill
              , view = viewMarkIncompleteButton
              }
            ]
        }


makeTableData : Time.Zone -> List Event -> List TableData
makeTableData zone events =
    List.foldl (addEventToTableDataDict zone) Dict.empty events
        |> Dict.values
        |> (\a -> (++) a [ List.foldl (addEventToTableData zone) (defaultTableData "Total") events |> (\data -> { data | isTotal = True }) ])


addEventToTableDataDict : Time.Zone -> Event -> Dict String TableData -> Dict String TableData
addEventToTableDataDict zone event dict =
    Dict.update event.name (Maybe.withDefault (defaultTableData event.name) >> addEventToTableData zone event >> Just) dict


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


addEventToTableData : Time.Zone -> Event -> TableData -> TableData
addEventToTableData zone event data =
    case event.startDate |> Date.fromPosix zone |> Date.weekday of
        Time.Mon ->
            { data | monday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.monday }

        Time.Tue ->
            { data | tuesday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.tuesday }

        Time.Wed ->
            { data | wednesday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.wednesday }

        Time.Thu ->
            { data | thursday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.thursday }

        Time.Fri ->
            { data | friday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.friday }

        Time.Sat ->
            { data | monday = Dict.update event.colorId (Maybe.withDefault 0 >> (+) event.minutes >> Just) data.monday }

        Time.Sun ->
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
        Element.el [ Font.color <| convertColor Color.lightCharcoal ]

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
                                Element.el [ Element.centerY, width <| Element.px halfGutter, Element.height <| Element.px halfGutter, Background.color <| convertColor color ] Element.none
                            )
                        |> Maybe.withDefault (Element.text <| colorId ++ ":")
                    , Element.text <| String.fromFloat (toFloat minutes / 60)
                    ]
            )
        |> Element.row [ spacing halfGutter, width fill, Element.centerY ]


viewNewInstanceButton : TableData -> Element Msg
viewNewInstanceButton data =
    case data.isTotal of
        True ->
            Element.none

        False ->
            Input.button (secondaryButton Color.darkGreen [])
                { onPress = Just <| NewInstance data.client
                , label = Element.text "New Instance"
                }


viewMarkCompleteButton : TableData -> Element Msg
viewMarkCompleteButton data =
    case data.isTotal of
        True ->
            Element.none

        False ->
            Input.button (secondaryButton Color.blue [])
                { onPress = Just <| MarkComplete data.client
                , label = Element.text "Complete"
                }


viewMarkIncompleteButton : TableData -> Element Msg
viewMarkIncompleteButton data =
    case data.isTotal of
        True ->
            Element.none

        False ->
            Input.button (secondaryButton Color.blue [])
                { onPress = Just <| MarkIncomplete data.client
                , label = Element.text "Incomplete"
                }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (5 * 1000) RxCurrentTime
        , Time.every (60 * 60 * 1000) (always RefreshEvents)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
