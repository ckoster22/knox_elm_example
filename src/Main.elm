module Main exposing (main)

import Html exposing (Html, button, div, fieldset, input, li, span, text, ul)
import Html.Attributes exposing (style, type_, value)
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


type RemoteData a b
    = Initial
    | Retrieving
    | Success a
    | Failure b -- Based on Kris Jenkins' remote data package
    | ZeroState


type alias Model =
    { userInput : UserInputModel
    , labelData : LabelData
    }


type alias UserInputModel =
    { repo : String }


type alias LabelData =
    RemoteData (List String) String


type Msg
    = TypeInput String
    | Retrieve
    | RetrieveSuccess (List String)
    | RetrieveError String


init : ( Model, Cmd Msg )
init =
    Model (UserInputModel "") Initial ! []



-- Views


view : Model -> Html Msg
view model =
    div
        []
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
    case labelData of
        Initial ->
            div [] [ text "Please enter a github repo in the format of \"account/repoName\"" ]

        Retrieving ->
            div [] [ text "Please wait.." ]

        Success results ->
            ul [] <| List.map resultListItem results

        Failure error ->
            div [] [ text error ]

        ZeroState ->
            div [] [ text "There aren't any labels for this github repository!" ]


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
            { model | labelData = Retrieving }
                |> andMsg (fetchRepos model.userInput)

        RetrieveError error ->
            { model | labelData = Failure error }
                |> noMsg

        RetrieveSuccess results ->
            if List.length results == 0 then
                { model | labelData = ZeroState }
                    |> noMsg
            else
                { model | labelData = Success results }
                    |> noMsg



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
