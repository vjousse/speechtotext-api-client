module Pages.Home_ exposing (Model, Msg(..), page)

import Auth exposing (User)
import Domain.AsrModel exposing (AsrModel)
import Domain.Config exposing (Config, ServerUrl)
import Domain.Info exposing (Info)
import Domain.MediaFile exposing (MediaFile)
import Domain.MediaFileList exposing (MediaFileList)
import Domain.Notification as DN exposing (Notification)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Gen.Params.Home_ exposing (Params)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode as Decode
import Page
import Request
import Requests.AsrModel
import Requests.Info
import Requests.MediaFile
import Requests.Upload
import Requests.Util exposing (errorToString)
import Shared
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Time exposing (Posix)
import UI
import Util.DateFormat
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.element
        (\user ->
            { init = init user shared
            , update = update user
            , view = view user
            , subscriptions = subscriptions
            }
        )



-- INIT


type alias Model =
    { asrModels : List AsrModel
    , files : List File
    , mediaFiles : List MediaFile
    , totalCount : Int
    , offset : Int
    , limit : Int
    , maxAround : Int
    , info : Maybe Info
    , modalOpened : Bool
    , uploadStatus : UploadStatus
    , lastUpdate : Maybe Posix
    , notification : Maybe Notification
    , config : Config
    , currentAsrModelId : Maybe Int
    }


init : User -> Shared.Model -> ( Model, Cmd Msg )
init user sharedModel =
    let
        offset =
            0

        limit =
            sharedModel.config.limit
    in
    ( { asrModels = []
      , files = []
      , mediaFiles = []
      , totalCount = 0
      , offset = offset
      , limit = limit
      , maxAround = 3
      , info = Nothing
      , modalOpened = False
      , uploadStatus = Waiting
      , lastUpdate = Nothing
      , notification = Nothing
      , config = sharedModel.config
      , currentAsrModelId = Nothing
      }
    , Cmd.batch
        [ Requests.Info.get sharedModel.config.serverUrl user GotInfo
        , Requests.AsrModel.get sharedModel.config.serverUrl user GotAsrModels
        , Requests.MediaFile.getAll sharedModel.config.serverUrl user offset limit GotMediaFiles
        ]
    )



-- UPDATE


type Msg
    = ClearNotification
    | FileRequested
    | FilesSelected File (List File)
    | Uploaded (Result Http.Error ())
    | GotAsrModels (Result Http.Error (List AsrModel))
    | GotInfo (Result Http.Error Info)
    | GotMediaFiles (Result Http.Error MediaFileList)
    | GotProgress Http.Progress
    | ToggleUploadFile
    | CloseUploadedModal
    | GetMediaFiles Int Int
    | Tick Time.Posix
    | SetAsrModel String
    | DownloadUrl String


type UploadStatus
    = Waiting
    | Uploading Float
    | UploadSuccess
    | UploadFailed String


update : User -> Msg -> Model -> ( Model, Cmd Msg )
update user msg model =
    case msg of
        DownloadUrl url ->
            ( model, Download.url url )

        ClearNotification ->
            ( { model | notification = Nothing }, Cmd.none )

        FileRequested ->
            let
                mimetypes =
                    case model.info of
                        Just info ->
                            info.allowedMimetypes

                        Nothing ->
                            [ "*" ]
            in
            ( model
            , Select.files mimetypes FilesSelected
            )

        FilesSelected firstFile otherFiles ->
            let
                allFiles =
                    firstFile :: otherFiles
            in
            ( { model | files = allFiles, uploadStatus = Uploading 0 }
            , case model.currentAsrModelId of
                Just currentAsrModelId ->
                    Requests.Upload.upload model.config.serverUrl allFiles currentAsrModelId user Uploaded

                _ ->
                    Cmd.none
            )

        ToggleUploadFile ->
            ( { model | modalOpened = not model.modalOpened }, Cmd.none )

        CloseUploadedModal ->
            ( { model | uploadStatus = Waiting }, Cmd.none )

        Uploaded result ->
            case result of
                Ok _ ->
                    ( { model | modalOpened = False, uploadStatus = UploadSuccess }
                    , Requests.MediaFile.getAll model.config.serverUrl user 0 model.limit GotMediaFiles
                    )

                Err e ->
                    ( { model | modalOpened = False, uploadStatus = UploadFailed (errorToString e) }
                    , Cmd.none
                    )

        GotAsrModels result ->
            case result of
                Ok models ->
                    let
                        currentAsrModelId =
                            case model.currentAsrModelId of
                                Just m ->
                                    Just m

                                -- If there is no model selected, use the first
                                -- one as default
                                Nothing ->
                                    Maybe.map (\m -> m.id) <| List.head models
                    in
                    ( { model | asrModels = models, currentAsrModelId = currentAsrModelId }, Cmd.none )

                Err e ->
                    ( { model
                        | notification =
                            Just
                                { title = "Http Error"
                                , subtitle = Just (Requests.Util.errorToString e)
                                , type_ = DN.Error
                                }
                      }
                    , Cmd.none
                    )

        GotInfo result ->
            case result of
                Ok info ->
                    ( { model | info = Just info }, Cmd.none )

                Err e ->
                    ( { model
                        | notification =
                            Just
                                { title = "Http Error"
                                , subtitle = Just (Requests.Util.errorToString e)
                                , type_ = DN.Error
                                }
                      }
                    , Cmd.none
                    )

        GetMediaFiles offset limit ->
            ( model, Requests.MediaFile.getAll model.config.serverUrl user offset limit GotMediaFiles )

        GotMediaFiles result ->
            case result of
                Ok files ->
                    ( { model | mediaFiles = files.files, totalCount = files.totalCount, offset = files.offset, limit = files.limit }, Cmd.none )

                Err e ->
                    ( { model
                        | notification =
                            Just
                                { title = "Http Error"
                                , subtitle = Just (Requests.Util.errorToString e)
                                , type_ = DN.Error
                                }
                      }
                    , Cmd.none
                    )

        GotProgress progress ->
            case progress of
                Http.Sending p ->
                    ( { model | uploadStatus = Uploading (Http.fractionSent p) }, Cmd.none )

                Http.Receiving _ ->
                    ( model, Cmd.none )

        SetAsrModel id ->
            ( { model | currentAsrModelId = String.toInt id }, Cmd.none )

        Tick time ->
            ( { model | lastUpdate = Just time }
            , Requests.MediaFile.getAll model.config.serverUrl user model.offset model.limit GotMediaFiles
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 5000 Tick
        , Http.track "upload" GotProgress
        ]



-- VIEW


uploadFinishedModal : UploadStatus -> Html Msg
uploadFinishedModal status =
    if status == Waiting then
        text ""

    else
        let
            title =
                if status == UploadSuccess then
                    "Transcription successfully started"

                else
                    "Error while uploading the file"

            message =
                case status of
                    UploadSuccess ->
                        "You can go back to your dashboard and check for the transcription progress"

                    UploadFailed e ->
                        e

                    _ ->
                        ""
        in
        div
            [ class "fixed z-10 inset-0 overflow-y-auto"
            , attribute "aria-labelledby" "modal-title"
            , attribute "role" "dialog"
            , attribute "aria-modal" "true"
            ]
            [ div
                [ class "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0"
                ]
                [ div
                    [ class "fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity"
                    , attribute "aria-hidden" "true"
                    , onClick CloseUploadedModal
                    ]
                    []
                , span
                    [ class "hidden sm:inline-block sm:align-middle sm:h-screen"
                    , attribute "aria-hidden" "true"
                    ]
                    [ text "\u{200B}" ]
                , div
                    [ class "inline-block align-bottom bg-white rounded-lg px-4 pt-5 pb-4 text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-sm sm:w-full sm:p-6"
                    ]
                    [ div []
                        [ div
                            [ class
                                ("mx-auto flex items-center justify-center h-12 w-12 rounded-full "
                                    ++ (if status == UploadSuccess then
                                            "bg-green-100"

                                        else
                                            "bg-red-100"
                                       )
                                )
                            ]
                            [ {- Heroicon name: outline/check -}
                              svg
                                [ SvgAttr.class
                                    ("h-6 w-6 "
                                        ++ (if status == UploadSuccess then
                                                "text-green-600"

                                            else
                                                "text-red-600"
                                           )
                                    )
                                , SvgAttr.fill "none"
                                , SvgAttr.viewBox "0 0 24 24"
                                , SvgAttr.stroke "currentColor"
                                , attribute "aria-hidden" "true"
                                ]
                                [ if status == UploadSuccess then
                                    path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M5 13l4 4L19 7"
                                        ]
                                        []

                                  else
                                    path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
                                        ]
                                        []
                                ]
                            ]
                        , div
                            [ class "mt-3 text-center sm:mt-5"
                            ]
                            [ h3
                                [ class "text-lg leading-6 font-medium text-gray-900"
                                , id "modal-title"
                                ]
                                [ text title ]
                            , div
                                [ class "mt-2"
                                ]
                                [ p
                                    [ class "text-sm text-gray-500"
                                    ]
                                    [ text message ]
                                ]
                            ]
                        ]
                    , div
                        [ class "mt-5 sm:mt-6"
                        ]
                        [ button
                            [ type_ "button"
                            , class "inline-flex justify-center w-full rounded-md border border-transparent shadow-sm px-4 py-2 bg-indigo-600 text-base font-medium text-white hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:text-sm"
                            , onClick CloseUploadedModal
                            ]
                            [ text "Go back to dashboard" ]
                        ]
                    ]
                ]
            ]


uploadModal : Model -> Html Msg
uploadModal model =
    div
        [ class "fixed z-10 inset-0 overflow-y-auto"
        , attribute "aria-labelledby" "modal-title"
        , attribute "role" "dialog"
        , attribute "aria-modal" "true"
        ]
        [ div
            [ class "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0"
            ]
            [ div
                [ class "fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity"
                , attribute "aria-hidden" "true"
                ]
                []
            , span
                [ class "hidden sm:inline-block sm:align-middle sm:h-screen"
                , attribute "aria-hidden" "true"
                ]
                [ text "\u{200B}" ]
            , div
                [ class "inline-block align-bottom bg-white rounded-lg px-4 pt-5 pb-4 text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-lg sm:w-full sm:p-6"
                ]
                [ div
                    [ class "sm:flex sm:items-start"
                    ]
                    [ div
                        [ class "mx-auto flex-shrink-0 flex items-center justify-center h-12 w-12 rounded-full bg-green-100 sm:mx-0 sm:h-10 sm:w-10"
                        ]
                        [ svg
                            [ SvgAttr.class "h-6 w-6 text-green-600"
                            , SvgAttr.fill "none"
                            , SvgAttr.viewBox "0 0 24 24"
                            , SvgAttr.stroke "currentColor"
                            ]
                            [ path
                                [ SvgAttr.strokeLinecap "round"
                                , SvgAttr.strokeLinejoin "round"
                                , SvgAttr.strokeWidth "2"
                                , SvgAttr.d "M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4-4m0 0L8 8m4-4v12"
                                ]
                                []
                            ]
                        ]
                    , div
                        [ class "mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left" ]
                        (case model.uploadStatus of
                            Waiting ->
                                [ h3 [ class "text-lg leading-6 font-medium text-gray-900", id "modal-title" ] [ text "Upload a file & start a transcription" ]
                                , div
                                    [ class "mt-2"
                                    ]
                                    [ p
                                        [ class "text-sm text-gray-500"
                                        ]
                                        [ text <|
                                            "Supported mimetypes are: "
                                                ++ (case model.info of
                                                        Just info ->
                                                            String.join ", " info.allowedMimetypes

                                                        Nothing ->
                                                            "unknown"
                                                   )
                                        ]
                                    ]
                                , div [ class "mt-2 mb-2 text-sm text-gray-500" ]
                                    [ label
                                        [ for "location"
                                        ]
                                        [ text "Choose your model" ]
                                    , select
                                        [ id "location"
                                        , name "location"
                                        , class "mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md text-gray-900"
                                        , on "change" (Decode.map SetAsrModel targetValue)
                                        ]
                                        (List.map
                                            (\asrModel ->
                                                option
                                                    [ value <| String.fromInt asrModel.id
                                                    , selected
                                                        (case model.currentAsrModelId of
                                                            Just m ->
                                                                asrModel.id == m

                                                            _ ->
                                                                False
                                                        )
                                                    ]
                                                    [ text <| asrModel.description ++ " (" ++ asrModel.label ++ ")" ]
                                            )
                                            model.asrModels
                                        )
                                    ]
                                ]

                            Uploading fraction ->
                                [ h3 [ class "text-lg leading-6 font-medium text-gray-900", id "modal-title" ] [ text "Transcription in progress: ", text (String.fromInt (round (100 * fraction)) ++ "%") ] ]

                            _ ->
                                [ text "" ]
                        )
                    ]
                , if model.uploadStatus == Waiting then
                    div
                        [ class "mt-5 sm:mt-4 sm:flex sm:flex-row-reverse"
                        ]
                        [ button
                            [ type_ "button"
                            , onClick FileRequested
                            , class "w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-4 py-2 bg-purple-600 text-base font-medium text-white hover:bg-purple-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-purple-500 sm:ml-3 sm:w-auto sm:text-sm"
                            ]
                            [ text "Choose file(s) & transcribe" ]
                        , button
                            [ type_ "button"
                            , onClick ToggleUploadFile
                            , class "mt-3 w-full inline-flex justify-center rounded-md border border-gray-300 shadow-sm px-4 py-2 bg-white text-base font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:mt-0 sm:w-auto sm:text-sm"
                            ]
                            [ text "Cancel" ]
                        ]

                  else
                    text ""
                ]
            ]
        ]


view : User -> Model -> View Msg
view user model =
    { title = "Homepage"
    , body =
        UI.layout user
            model.config.serverUrl
            [ div [ class "border-b border-gray-200 px-4 py-4 sm:flex sm:items-center sm:justify-between sm:px-6 lg:px-8" ]
                [ div [ class "flex-1 min-w-0" ]
                    [ h1 [ class "text-lg font-medium leading-6 text-gray-900 sm:truncate" ]
                        [ text "Files" ]
                    ]
                , div [ class "mt-4 flex sm:mt-0 sm:ml-4" ]
                    [ button [ onClick ToggleUploadFile, class "order-0 inline-flex items-center px-4 py-2 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-purple-600 hover:bg-purple-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-purple-500 sm:order-1 sm:ml-3", type_ "button" ]
                        [ text "Transcribe a file" ]
                    ]
                ]
            , div
                [ class "flex flex-col px-4 py-4 sm:px-6 lg:px-8"
                ]
                [ div
                    [ class "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8"
                    ]
                    [ div
                        [ class "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8"
                        ]
                        [ div
                            [ class "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg"
                            ]
                            [ navigation model.totalCount model.offset model.limit False model.maxAround model.lastUpdate user
                            , table
                                [ class "min-w-full divide-y divide-gray-200"
                                ]
                                [ thead
                                    [ class "bg-gray-50"
                                    ]
                                    [ tr []
                                        [ th
                                            [ scope "col"
                                            , class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                                            ]
                                            [ text "Id" ]
                                        , th
                                            [ scope "col"
                                            , class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-widerz"
                                            ]
                                            [ text "Filename" ]
                                        , th
                                            [ scope "col"
                                            , class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                                            ]
                                            [ text "UUID" ]
                                        , th
                                            [ scope "col"
                                            , class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                                            ]
                                            [ text "Created at" ]
                                        , th
                                            [ scope "col"
                                            , class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"
                                            ]
                                            [ text "Status" ]
                                        , th
                                            [ scope "col"
                                            , class "relative px-6 py-3"
                                            ]
                                            [ span
                                                [ class "sr-only"
                                                ]
                                                [ text "Download" ]
                                            ]
                                        ]
                                    ]
                                , tbody []
                                    (List.indexedMap
                                        (\i file -> fileDetail file (modBy 2 i == 0) model.config.serverUrl)
                                        model.mediaFiles
                                    )
                                ]
                            , navigation model.totalCount model.offset model.limit True model.maxAround model.lastUpdate user
                            ]
                        ]
                    ]
                ]
            , if model.modalOpened then
                uploadModal model

              else
                uploadFinishedModal model.uploadStatus
            , UI.displayNotification model.notification ClearNotification
            ]
    }


navigation : Int -> Int -> Int -> Bool -> Int -> Maybe Posix -> User -> Html Msg
navigation totalCount offset limit showResultsNumbers maxAround lastUpdate user =
    let
        previous =
            offset > 0

        next =
            offset + limit < totalCount

        totalPages =
            ceiling <|
                toFloat totalCount
                    / toFloat limit

        currentPage =
            ((toFloat offset / toFloat limit) |> truncate) + 1
    in
    div
        [ class "bg-white px-4 py-3 flex items-center justify-between border-t border-gray-200 sm:px-6"
        ]
        [ div
            [ class "flex-1 flex justify-between sm:hidden"
            ]
            [ if previous then
                a
                    [ href "#"
                    , class "relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                    ]
                    [ text "Previous" ]

              else
                text ""
            , if next then
                a
                    [ href "#"
                    , class "ml-3 relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                    ]
                    [ text "Next" ]

              else
                text ""
            ]
        , div
            [ class "hidden sm:flex-1 sm:flex sm:items-center sm:justify-between"
            ]
            [ div []
                [ if showResultsNumbers then
                    p
                        [ class "text-sm text-gray-700"
                        ]
                        [ text "Showing "
                        , span
                            [ class "font-semibold"
                            ]
                            [ text <|
                                if totalCount == 0 then
                                    "0"

                                else
                                    String.fromInt (offset + 1)
                            ]
                        , text " to "
                        , span
                            [ class "font-semibold"
                            ]
                            [ text <|
                                if totalCount > (offset + limit) then
                                    String.fromInt (offset + limit)

                                else
                                    String.fromInt totalCount
                            ]
                        , text " of "
                        , span
                            [ class "font-semibold"
                            ]
                            [ text <| String.fromInt totalCount ]
                        , text " results."
                        , case lastUpdate of
                            Just time ->
                                span []
                                    [ text " Last update: "
                                    , span
                                        [ class "font-semibold" ]
                                        [ text <| Util.DateFormat.formatDateUtc time
                                        ]
                                    ]

                            _ ->
                                text ""
                        ]

                  else
                    text ""
                ]
            , div []
                [ nav
                    [ class "relative z-0 inline-flex rounded-md shadow-sm -space-x-px"
                    , attribute "aria-label" "Pagination"
                    ]
                    ([ if previous then
                        a
                            [ href "#"
                            , class "relative inline-flex items-center px-2 py-2 rounded-l-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50"
                            , onClick (GetMediaFiles (offset - limit) limit)
                            ]
                            [ span
                                [ class "sr-only"
                                ]
                                [ text "Previous" ]
                            , svg
                                [ SvgAttr.class "h-5 w-5"
                                , attribute "x-description" "Heroicon name: solid/chevron-left"
                                , SvgAttr.viewBox "0 0 20 20"
                                , SvgAttr.fill "currentColor"
                                , attribute "aria-hidden" "true"
                                ]
                                [ path
                                    [ SvgAttr.fillRule "evenodd"
                                    , SvgAttr.d "M12.707 5.293a1 1 0 010 1.414L9.414 10l3.293 3.293a1 1 0 01-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z"
                                    , SvgAttr.clipRule "evenodd"
                                    ]
                                    []
                                ]
                            ]

                       else
                        button
                            [ class "relative inline-flex items-center px-2 py-2 rounded-l-md border border-gray-300 bg-white text-sm font-medium text-gray-100 cursor-default"
                            ]
                            [ span
                                [ class "sr-only"
                                ]
                                [ text "Previous" ]
                            , svg
                                [ SvgAttr.class "h-5 w-5"
                                , attribute "x-description" "Heroicon name: solid/chevron-left"
                                , SvgAttr.viewBox "0 0 20 20"
                                , SvgAttr.fill "currentColor"
                                , attribute "aria-hidden" "true"
                                ]
                                [ path
                                    [ SvgAttr.fillRule "evenodd"
                                    , SvgAttr.d "M12.707 5.293a1 1 0 010 1.414L9.414 10l3.293 3.293a1 1 0 01-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z"
                                    , SvgAttr.clipRule "evenodd"
                                    ]
                                    []
                                ]
                            ]
                     ]
                        ++ generatePages currentPage totalPages offset limit maxAround
                        ++ [ if next then
                                a
                                    [ href "#"
                                    , class "relative inline-flex items-center px-2 py-2 rounded-r-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50"
                                    , onClick (GetMediaFiles (offset + limit) limit)
                                    ]
                                    [ span
                                        [ class "sr-only"
                                        ]
                                        [ text "Next" ]
                                    , svg
                                        [ SvgAttr.class "h-5 w-5"
                                        , attribute "x-description" "Heroicon name: solid/chevron-right"
                                        , SvgAttr.viewBox "0 0 20 20"
                                        , SvgAttr.fill "currentColor"
                                        , attribute "aria-hidden" "true"
                                        ]
                                        [ path
                                            [ SvgAttr.fillRule "evenodd"
                                            , SvgAttr.d "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                                            , SvgAttr.clipRule "evenodd"
                                            ]
                                            []
                                        ]
                                    ]

                             else
                                a
                                    [ class "relative inline-flex items-center px-2 py-2 rounded-r-md border border-gray-300 bg-white text-sm font-medium text-gray-100 cursor-default"
                                    ]
                                    [ span
                                        [ class "sr-only"
                                        ]
                                        [ text "Next" ]
                                    , svg
                                        [ SvgAttr.class "h-5 w-5"
                                        , attribute "x-description" "Heroicon name: solid/chevron-right"
                                        , SvgAttr.viewBox "0 0 20 20"
                                        , SvgAttr.fill "currentColor"
                                        , attribute "aria-hidden" "true"
                                        ]
                                        [ path
                                            [ SvgAttr.fillRule "evenodd"
                                            , SvgAttr.d "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                                            , SvgAttr.clipRule "evenodd"
                                            ]
                                            []
                                        ]
                                    ]
                           ]
                    )
                ]
            ]
        ]


generatePages : Int -> Int -> Int -> Int -> Int -> List (Html Msg)
generatePages currentPage totalPages offset limit maxAround =
    let
        breakLeft =
            (currentPage - 2)
                > maxAround

        breakRight =
            (currentPage + maxAround + 1)
                < totalPages

        leftBoundary =
            if breakLeft then
                currentPage - maxAround

            else
                1

        rightBoundary =
            if breakRight then
                currentPage + maxAround

            else
                totalPages

        pages =
            List.range leftBoundary rightBoundary
    in
    (if breakLeft then
        [ linkToPage limit 1 (currentPage == 1), noPage ]

     else
        [ text "" ]
    )
        ++ List.map
            (\pageNumber -> linkToPage limit pageNumber (currentPage == pageNumber))
            pages
        ++ (if breakRight then
                [ noPage, linkToPage limit totalPages (currentPage == totalPages) ]

            else
                [ text "" ]
           )



--List.map
--    (\pageNumber ->
--        linkToPage limit pageNumber currentPage
--    )
--    pages


linkToPage : Int -> Int -> Bool -> Html Msg
linkToPage limit pageNumber currentPage =
    a
        ([ href "#"
         , onClick (GetMediaFiles (limit * (pageNumber - 1)) limit)
         , class
            (if currentPage then
                "z-10 bg-indigo-50 border-indigo-500 text-indigo-600 relative inline-flex items-center px-4 py-2 border text-sm font-medium"

             else
                "bg-white border-gray-300 text-gray-500 hover:bg-gray-50 relative inline-flex items-center px-4 py-2 border text-sm font-medium"
            )
         ]
            ++ (if currentPage then
                    [ attribute "aria-current" "page" ]

                else
                    []
               )
        )
        [ text <| String.fromInt pageNumber ]


noPage : Html Msg
noPage =
    button
        [ class
            "bg-white border-gray-300 text-gray-500 relative inline-flex items-center px-4 py-2 border text-sm font-medium cursor-default"
        ]
        [ text "..." ]


fileDetail : MediaFile -> Bool -> ServerUrl -> Html Msg
fileDetail mediaFile isOdd serverUrl =
    tr
        [ class
            (if isOdd then
                "bg-white"

             else
                "bg-gray-50"
            )
        ]
        [ td
            [ class "px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900"
            ]
            [ text <| String.fromInt mediaFile.id ]
        , td
            [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-500"
            ]
            [ text mediaFile.filename ]
        , td
            [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-500"
            ]
            [ text mediaFile.uuid ]
        , td
            [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-500"
            ]
            [ text <| Util.DateFormat.formatDateUtc mediaFile.createdAt ]
        , td
            [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-500"
            ]
            (displayStatus
                mediaFile
            )
        , td
            [ class "px-6 py-4 whitespace-nowrap text-right text-sm font-medium"
            ]
            (displayDownloadLinks
                mediaFile
                serverUrl
            )
        ]


displayDownloadLinks : MediaFile -> ServerUrl -> List (Html Msg)
displayDownloadLinks mediaFile serverUrl =
    List.map
        (\result ->
            let
                downloadUrl =
                    serverUrl ++ "/uploads/" ++ result.filename
            in
            a
                [ href "#"
                , class "text-indigo-600 hover:text-indigo-900"
                , onClick <| DownloadUrl downloadUrl
                ]
                [ text "Download transcript" ]
        )
        mediaFile.results


displayStatus : MediaFile -> List (Html msg)
displayStatus mediaFile =
    List.map
        (\task ->
            div [ class (getColorForStatus task.status) ]
                [ div [ class "flex items-center space-x-2" ]
                    [ case task.status of
                        "done" ->
                            svg
                                [ SvgAttr.class ("h-5 w-5 " ++ getColorForStatus task.status)
                                , SvgAttr.viewBox "0 0 20 20"
                                , SvgAttr.fill "currentColor"
                                ]
                                [ path
                                    [ SvgAttr.fillRule "evenodd"
                                    , SvgAttr.d "M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z"
                                    , SvgAttr.clipRule "evenodd"
                                    ]
                                    []
                                ]

                        "failed" ->
                            svg
                                [ SvgAttr.class ("h-5 w-5 " ++ getColorForStatus task.status)
                                , SvgAttr.viewBox "0 0 20 20"
                                , SvgAttr.fill "currentColor"
                                ]
                                [ path
                                    [ SvgAttr.fillRule "evenodd"
                                    , SvgAttr.d "M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z"
                                    , SvgAttr.clipRule "evenodd"
                                    ]
                                    []
                                ]

                        "enqueued" ->
                            svg
                                [ SvgAttr.class ("h-5 w-5 " ++ getColorForStatus task.status)
                                , SvgAttr.viewBox "0 0 20 20"
                                , SvgAttr.fill "currentColor"
                                ]
                                [ path
                                    [ SvgAttr.fillRule "evenodd"
                                    , SvgAttr.d "M10 18a8 8 0 100-16 8 8 0 000 16zM7 9H5v2h2V9zm8 0h-2v2h2V9zM9 9h2v2H9V9z"
                                    , SvgAttr.clipRule "evenodd"
                                    ]
                                    []
                                ]

                        "running" ->
                            svg
                                [ SvgAttr.class ("h-5 w-5 " ++ getColorForStatus task.status)
                                , SvgAttr.viewBox "0 0 20 20"
                                , SvgAttr.fill "currentColor"
                                ]
                                [ path
                                    [ SvgAttr.fillRule "evenodd"
                                    , SvgAttr.d "M10 18a8 8 0 100-16 8 8 0 000 16zM7 9H5v2h2V9zm8 0h-2v2h2V9zM9 9h2v2H9V9z"
                                    , SvgAttr.clipRule "evenodd"
                                    ]
                                    []
                                ]

                        "delayed" ->
                            svg
                                [ SvgAttr.class ("h-5 w-5 " ++ getColorForStatus task.status)
                                , SvgAttr.viewBox "0 0 20 20"
                                , SvgAttr.fill "currentColor"
                                ]
                                [ path
                                    [ SvgAttr.fillRule "evenodd"
                                    , SvgAttr.d "M10 18a8 8 0 100-16 8 8 0 000 16zm1-12a1 1 0 10-2 0v4a1 1 0 00.293.707l2.828 2.829a1 1 0 101.415-1.415L11 9.586V6z"
                                    , SvgAttr.clipRule "evenodd"
                                    ]
                                    []
                                ]

                        "skipped" ->
                            svg
                                [ SvgAttr.class ("h-5 w-5 " ++ getColorForStatus task.status)
                                , SvgAttr.viewBox "0 0 20 20"
                                , SvgAttr.fill "currentColor"
                                ]
                                [ path
                                    [ SvgAttr.fillRule "evenodd"
                                    , SvgAttr.d "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7 4a1 1 0 11-2 0 1 1 0 012 0zm-1-9a1 1 0 00-1 1v4a1 1 0 102 0V6a1 1 0 00-1-1z"
                                    , SvgAttr.clipRule "evenodd"
                                    ]
                                    []
                                ]

                        _ ->
                            text ""
                    , span [] [ text task.status ]
                    ]
                ]
        )
        mediaFile.tasks


getColorForStatus : String -> String
getColorForStatus status =
    -- Don't optimize it: if postcss doesn't see the full class name
    -- in the javascript, it will strip it from the css
    case status of
        "done" ->
            "text-green-500"

        "failed" ->
            "text-red-500"

        "enqueued" ->
            "text-indigo-500"

        "delayed" ->
            "text-blue-500"

        "running" ->
            "text-yellow-500"

        _ ->
            "text-gray-500"
