module Main exposing (..)

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
import Http
import Json.Decode as JD
import Json.Decode.Extra as JDExtra
import Json.Encode as JE
import Navigation
import OAuth
import OAuth.Implicit
import RemoteData exposing (RemoteData(..), WebData)
import Return
import Task exposing (Task)
import Time


---- MODEL ----


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
    { calendar : WebData String
    , today : Maybe Date
    , colors : Dict String EventColor
    , events : WebData (List Event)
    , weeksBack : Int
    , oauthToken : Maybe OAuth.ResponseToken
    , location : Navigation.Location
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    Return.singleton
        { calendar = NotAsked
        , today = Nothing
        , colors = Dict.empty
        , events = NotAsked
        , weeksBack = 0
        , oauthToken = OAuth.Implicit.parse location |> Result.toMaybe
        , location = location
        }
        |> Return.command (Task.perform RxDate Date.now)
        |> Return.effect_ (.oauthToken >> Maybe.map getCalendarList >> Maybe.withDefault Cmd.none)
        |> Return.effect_ (.oauthToken >> Maybe.map getColors >> Maybe.withDefault Cmd.none)
        |> Return.command (Navigation.modifyUrl location.origin)



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
    | RxDate Date
    | RefreshEvents
    | NewEvent String
    | MarkComplete String
    | MarkIncomplete String
    | GoBackOneWeek
    | GoForwardOneWeek
    | GoToCurrentWeek
    | OAuthAuthorize
    | NavigationNoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RxPrimaryCalendarId data ->
            Return.singleton { model | calendar = data }
                |> Return.effect_ loadEvents

        RxColors data ->
            Return.singleton { model | colors = RemoteData.withDefault model.colors data }

        RxEvents data ->
            Return.singleton { model | events = data }

        RxEventAck _ ->
            Return.singleton model
                |> Return.effect_ loadEvents

        RxDate today ->
            ( { model | today = Just today }, Cmd.none )

        RefreshEvents ->
            Return.singleton model
                |> Return.effect_ loadEvents

        NewEvent eventName ->
            Return.singleton model
                |> Return.effect_ (createEvent eventName)

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
                |> Return.effect_ (.location >> login)

        NavigationNoOp ->
            Return.singleton model



---- COMMANDS ----


login : Navigation.Location -> Cmd Msg
login location =
    OAuth.Implicit.authorize
        { clientId = "159193416244-l4tsfgdhbn402qq57ajahsf3cu41vno0.apps.googleusercontent.com"
        , redirectUri = location.origin ++ location.pathname
        , responseType = OAuth.Token
        , scope = [ "email", "profile", "https://www.googleapis.com/auth/calendar" ]
        , state = Nothing
        , url = "https://accounts.google.com/o/oauth2/v2/auth"
        }


getCalendarList : OAuth.ResponseToken -> Cmd Msg
getCalendarList response =
    let
        calendarListDecoder =
            JD.field "items" <|
                JD.list
                    (JD.map2 (,)
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
        , headers = OAuth.use response.token []
        , withCredentials = False
        , url = "https://www.googleapis.com/calendar/v3/users/me/calendarList"
        , expect = Http.expectJson (calendarListDecoder |> JD.andThen findPrimaryCalendar)
        , timeout = Nothing
        }
        |> RemoteData.sendRequest
        |> Cmd.map RxPrimaryCalendarId


getColors : OAuth.ResponseToken -> Cmd Msg
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
        , headers = OAuth.use response.token []
        , withCredentials = False
        , url = "https://www.googleapis.com/calendar/v3/colors"
        , expect = Http.expectJson decoder
        , timeout = Nothing
        }
        |> RemoteData.sendRequest
        |> Cmd.map RxColors


loadEvents : Model -> Cmd Msg
loadEvents model =
    Maybe.map3 (getEventsRequest model.weeksBack)
        model.today
        (RemoteData.toMaybe model.calendar)
        model.oauthToken
        |> Maybe.map (RemoteData.sendRequest >> Cmd.map RxEvents)
        |> Maybe.withDefault Cmd.none


getEventsRequest : Int -> Date -> String -> OAuth.ResponseToken -> Http.Request (List Event)
getEventsRequest weeksBack today calendarId response =
    let
        decoder =
            JD.field "items" (JD.map (List.filterMap identity) <| JD.list eventDecoder)

        eventDecoder =
            JD.maybe <|
                JD.map5 makeEvent
                    (JD.field "summary" JD.string)
                    (JD.at [ "start", "dateTime" ] JDExtra.date)
                    (JD.at [ "end", "dateTime" ] JDExtra.date)
                    (JD.field "id" JD.string)
                    (JD.field "colorId" JD.string)

        makeEvent name start end id color =
            Event name start end id color (Date.Extra.diff Date.Extra.Minute start end)
    in
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.use response.token []
        , withCredentials = False
        , url =
            "https://www.googleapis.com/calendar/v3/calendars/"
                ++ calendarId
                ++ "/events?singleEvents=true&timeMin="
                ++ (Date.Extra.toIsoString <| Date.Extra.floor Date.Extra.Saturday <| Date.Extra.add Date.Extra.Week (-1 * weeksBack) today)
                ++ "&timeMax="
                ++ (Date.Extra.toIsoString <| Date.Extra.ceiling Date.Extra.Saturday <| Date.Extra.add Date.Extra.Week (-1 * weeksBack) today)
        , expect = Http.expectJson decoder
        , timeout = Nothing
        }


createEvent : String -> Model -> Cmd Msg
createEvent eventName model =
    Maybe.map3 (createEventRequest eventName)
        model.today
        (RemoteData.toMaybe model.calendar)
        model.oauthToken
        |> Maybe.map (RemoteData.sendRequest >> Cmd.map RxEventAck)
        |> Maybe.withDefault Cmd.none


createEventRequest : String -> Date -> String -> OAuth.ResponseToken -> Http.Request String
createEventRequest eventName today calendarId oauth =
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
                        , ( "overrides", JE.list [] )
                        ]
                  )
                , ( "start", JE.object [ ( "dateTime", JE.string <| Date.Extra.toIsoString <| Date.Extra.add Date.Extra.Hour -1 <| Date.Extra.floor Date.Extra.Hour today ) ] )
                , ( "end", JE.object [ ( "dateTime", JE.string <| Date.Extra.toIsoString <| Date.Extra.floor Date.Extra.Hour today ) ] )
                ]
    in
    Http.request
        { method = "POST"
        , body = Http.jsonBody bodyValue
        , headers = OAuth.use oauth.token []
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
        model.oauthToken
        |> Maybe.andThen
            (\task ->
                Maybe.map3
                    (\today calendar oauth ->
                        task
                            |> Task.andThen
                                (\_ ->
                                    getEventsRequest model.weeksBack today calendar oauth
                                        |> Http.toTask
                                        |> RemoteData.fromTask
                                )
                    )
                    model.today
                    (RemoteData.toMaybe model.calendar)
                    model.oauthToken
            )
        |> Maybe.map (Task.perform RxEvents)
        |> Maybe.withDefault Cmd.none


changeEventsColorHelper : String -> ColorId -> List Event -> String -> OAuth.ResponseToken -> Task Never (List (WebData String))
changeEventsColorHelper eventName newColor events calendarId oauth =
    events
        |> List.filter (.name >> (==) eventName)
        |> List.map (.id >> changeEventColorRequest calendarId newColor oauth >> Http.toTask >> RemoteData.fromTask)
        |> Task.sequence


changeEventColorRequest : String -> ColorId -> OAuth.ResponseToken -> String -> Http.Request String
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
        , headers = OAuth.use oauth.token []
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
            Element.text <| toString err

        Success thing ->
            viewFun thing


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
        , Input.button (secondaryButton Color.green [ Element.alignRight ])
            { onPress = Just OAuthAuthorize
            , label = Element.text "Begin OAuth"
            }
        , Element.el [ Element.alignRight ] <|
            viewWebData Element.text model.calendar
        ]


viewBody : Model -> Element Msg
viewBody model =
    Element.column [ Element.paddingXY gutter halfGutter, spacing halfGutter ]
        [ viewInputRow model
        , viewWebData (viewWeekTable model.colors) model.events
        ]


viewInputRow : Model -> Element Msg
viewInputRow model =
    Element.row [ Background.color Color.lightGray, Border.rounded quarterGutter, padding halfGutter, spacing halfGutter ]
        [ Input.button (secondaryButton Color.blue [ alignLeft ])
            { onPress = Just GoBackOneWeek
            , label = Element.text "<"
            }
        , Element.text <| "Week starting " ++ Maybe.withDefault "?" (Maybe.map (Date.Extra.toFormattedString "MM-dd-yyyy" << Date.Extra.floor Date.Extra.Monday << Date.Extra.add Date.Extra.Week (-1 * model.weeksBack)) model.today)
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



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (5 * Time.second) (Date.fromTime >> RxDate)
        , Time.every (60 * Time.minute) (always RefreshEvents)
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program (always NavigationNoOp)
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
