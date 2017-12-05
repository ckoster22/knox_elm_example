module Main exposing (main)

import Html exposing (Html, button, div, fieldset, input, li, span, text, ul)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Types


type alias Model =
    { userInput : UserInputModel
    , labelData : LabelData
    }


type alias UserInputModel =
    { repo : String }


type alias LabelData =
    { isRetrieving : Bool
    , retrievalError : Maybe String
    , results : List String
    }


type Msg
    = TypeInput String
    | Retrieve
    | RetrieveSuccess (List String)
    | RetrieveError String


init : ( Model, Cmd Msg )
init =
    let
        initialUserInputModel =
            UserInputModel ""

        initialLabelData =
            LabelData False Nothing []

        initialModel =
            Model initialUserInputModel initialLabelData
    in
    initialModel ! []



-- Views


view : Model -> Html Msg
view model =
    div
        [ class "exampleFieldset" ]
        [ formView model.userInput
        , button [ onClick Retrieve ] [ text "Search" ]
        , resultsView model.labelData
        ]
        |> elmReactorCssWorkaround


formView : UserInputModel -> Html Msg
formView { repo } =
    fieldset
        []
        [ input
            [ type_ "text", value repo, onInput TypeInput ]
            []
        ]


resultsView : LabelData -> Html Msg
resultsView labelData =
    if labelData.isRetrieving then
        div [] [ text "Please wait.." ]
    else if labelData.retrievalError /= Nothing then
        case labelData.retrievalError of
            Just error ->
                div [] [ text error ]

            Nothing ->
                div [] []
    else
        ul [] <| List.map resultListItem labelData.results


resultListItem : String -> Html Msg
resultListItem name =
    li
        []
        [ text name ]



-- Updates


noMsg : Model -> ( Model, Cmd Msg )
noMsg model =
    model ! []


andMsg : Cmd Msg -> Model -> ( Model, Cmd Msg )
andMsg cmd model =
    model ! [ cmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypeInput value ->
            { model | userInput = UserInputModel value }
                |> noMsg

        Retrieve ->
            model
                |> setIsRetrieving True
                |> andMsg (fetchRepos model.userInput)

        RetrieveError error ->
            model
                |> setIsRetrieving False
                |> setError (Just error)
                |> noMsg

        RetrieveSuccess results ->
            model
                |> setIsRetrieving False
                |> setResults results
                |> noMsg


setIsRetrieving : Bool -> Model -> Model
setIsRetrieving isRetrieving model =
    let
        labelData =
            model.labelData

        updatedLabelData =
            { labelData | isRetrieving = isRetrieving }
    in
    { model | labelData = updatedLabelData }


setError : Maybe String -> Model -> Model
setError error model =
    let
        labelData =
            model.labelData

        updatedLabelData =
            { labelData | retrievalError = error }
    in
    { model | labelData = updatedLabelData }


setResults : List String -> Model -> Model
setResults results model =
    let
        labelData =
            model.labelData

        updatedLabelData =
            { labelData | results = results }
    in
    { model | labelData = updatedLabelData }



-- HTTP


fetchRepos : UserInputModel -> Cmd Msg
fetchRepos { repo } =
    Http.getString ("https://api.github.com/repos/" ++ repo ++ "/labels")
        |> Http.send resultToMsg


resultToMsg : Result Http.Error String -> Msg
resultToMsg result =
    case Result.map (Decode.decodeString labelsDecoder) result of
        Ok decodedResult ->
            case decodedResult of
                Ok result ->
                    RetrieveSuccess result

                Err error ->
                    RetrieveError error

        Err _ ->
            RetrieveError "There was a problem retrieving github repo info"


labelsDecoder : Decode.Decoder (List String)
labelsDecoder =
    Decode.list labelDecoder


labelDecoder : Decode.Decoder String
labelDecoder =
    Decode.field "name" Decode.string



-- Workaround


elmReactorCssWorkaround : Html Msg -> Html Msg
elmReactorCssWorkaround appView =
    div
        []
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , appView
        ]
