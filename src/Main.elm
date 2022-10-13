port module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, form, input, label, span, text)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, placeholder, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Json.Decode as Json exposing (Decoder)
import Random
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, viewBox)


type alias Model =
    { newTodoText : String
    , newTodoId : Maybe Id
    , editingTodo : Maybe Todo
    , todos : List Todo
    , filter : Filter
    }


type alias Id =
    Int


type alias Todo =
    { id : Id
    , text : String
    , isComplete : Bool
    }


decodeTodo : Decoder Todo
decodeTodo =
    Json.map3 Todo
        (Json.field "id" Json.int)
        (Json.field "text" Json.string)
        (Json.field "isComplete" Json.bool)


type Filter
    = FilterAll
    | FilterIncomplete
    | FilterComplete


init : Json.Value -> ( Model, Cmd Msg )
init jsonValue =
    let
        todos : List Todo
        todos =
            Json.decodeValue Json.string jsonValue
                |> Result.andThen (Json.decodeString (Json.list decodeTodo))
                |> Result.withDefault []
    in
    ( { newTodoText = ""
      , newTodoId = Nothing
      , editingTodo = Nothing
      , todos = todos
      , filter = FilterAll
      }
    , generateNewTodoId
    )


type Msg
    = SetNewTodoText String
    | SetNewTodoId Id
    | AddNewTodo
    | ToggleTodo Id Bool
    | RemoveTodo Id
    | StartEditingTodo Id
    | SetEditingTodoText String
    | CancelEditingTodo
    | SaveEditingTodo
    | ToggleAllTodos Bool
    | SetFilter Filter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewTodoText newTodoText ->
            ( { model | newTodoText = newTodoText }, Cmd.none )

        SetNewTodoId newTodoId ->
            ( { model | newTodoId = Just newTodoId }, Cmd.none )

        AddNewTodo ->
            case model.newTodoId of
                Nothing ->
                    ( model, Cmd.none )

                Just newTodoId ->
                    let
                        newTodo : Todo
                        newTodo =
                            { id = newTodoId
                            , text = model.newTodoText
                            , isComplete = False
                            }

                        newTodos : List Todo
                        newTodos =
                            model.todos ++ [ newTodo ]
                    in
                    ( { model
                        | todos = newTodos
                        , newTodoText = ""
                        , newTodoId = Nothing
                      }
                    , Cmd.batch
                        [ generateNewTodoId
                        , saveTodos newTodos
                        ]
                    )

        ToggleTodo todoId isComplete ->
            let
                newTodos : List Todo
                newTodos =
                    updateTodo todoId
                        (\todo -> { todo | isComplete = isComplete })
                        model.todos
            in
            ( { model | todos = newTodos }, saveTodos newTodos )

        RemoveTodo todoId ->
            let
                newTodos : List Todo
                newTodos =
                    removeTodo todoId model.todos
            in
            ( { model | todos = newTodos }, saveTodos newTodos )

        StartEditingTodo todoId ->
            ( { model | editingTodo = findTodo todoId model.todos }, Cmd.none )

        SetEditingTodoText todoText ->
            ( { model
                | editingTodo =
                    Maybe.map (\todo -> { todo | text = todoText })
                        model.editingTodo
              }
            , Cmd.none
            )

        CancelEditingTodo ->
            ( { model | editingTodo = Nothing }, Cmd.none )

        SaveEditingTodo ->
            case model.editingTodo of
                Nothing ->
                    ( model, Cmd.none )

                Just editingTodo ->
                    let
                        newTodos : List Todo
                        newTodos =
                            updateTodo editingTodo.id
                                (\_ -> editingTodo)
                                model.todos
                    in
                    ( { model | todos = newTodos, editingTodo = Nothing }
                    , saveTodos newTodos
                    )

        ToggleAllTodos isComplete ->
            let
                newTodos : List Todo
                newTodos =
                    List.map (\todo -> { todo | isComplete = isComplete })
                        model.todos
            in
            ( { model | todos = newTodos }, saveTodos newTodos )

        SetFilter filter ->
            ( { model | filter = filter }, Cmd.none )


generateNewTodoId : Cmd Msg
generateNewTodoId =
    Random.int Random.minInt Random.maxInt
        |> Random.generate SetNewTodoId


updateTodo : Id -> (Todo -> Todo) -> List Todo -> List Todo
updateTodo id updater list =
    List.map
        (\x ->
            if x.id == id then
                updater x

            else
                x
        )
        list


removeTodo : Id -> List Todo -> List Todo
removeTodo id list =
    List.filterMap
        (\x ->
            if x.id == id then
                Nothing

            else
                Just x
        )
        list


findTodo : Id -> List Todo -> Maybe Todo
findTodo id list =
    List.filter (.id >> (==) id) list |> List.head


allTodosAreComplete : Model -> Bool
allTodosAreComplete model =
    List.all (\todo -> todo.isComplete) model.todos


view : Model -> Html Msg
view model =
    let
        filteredTodos : List Todo
        filteredTodos =
            List.filter
                (\todo ->
                    case model.filter of
                        FilterAll ->
                            True

                        FilterComplete ->
                            todo.isComplete

                        FilterIncomplete ->
                            not todo.isComplete
                )
                model.todos
    in
    div [ class "min-h-screen bg-black text-white p-3" ]
        [ div
            [ class "container mx-auto" ]
            [ form
                [ class "flex max-w-xl md:w-2/3 mx-auto mb-3"
                , onSubmit AddNewTodo
                ]
                [ input
                    [ class "grow py-2 px-3 bg-gray-800 rounded"
                    , onInput SetNewTodoText
                    , value model.newTodoText
                    , placeholder "Todo text"
                    ]
                    []
                , blueButton [ class "w-1/4" ] [ text "Add" ]
                ]
            , div [ class "flex gap-2 justify-center mb-3" ]
                [ viewFilter model.filter FilterAll "All"
                , viewFilter model.filter FilterIncomplete "Incomplete"
                , viewFilter model.filter FilterComplete "Complete"
                ]
            , div [ class "max-w-2xl mx-auto mb-3" ]
                [ label [ class "inline-flex gap-3 items-center" ]
                    [ input
                        [ type_ "checkbox"
                        , checked (allTodosAreComplete model)
                        , onCheck ToggleAllTodos
                        , class "w-5 h-5"
                        ]
                        []
                    , span [] [ text "Check all" ]
                    ]
                ]
            , viewTodos model.editingTodo filteredTodos
            ]
        ]


viewFilter : Filter -> Filter -> String -> Html Msg
viewFilter activeFilter filter filterText =
    button
        [ classList
            [ ( "text-cyan-400 border-b-2 border-cyan-400", filter == activeFilter )
            , ( "rounded hover:bg-gray-800", filter /= activeFilter )
            ]
        , class "px-3 py-2"
        , onClick (SetFilter filter)
        , disabled (filter == activeFilter)
        ]
        [ text filterText ]


blueButton : List (Attribute msg) -> List (Html msg) -> Html msg
blueButton attributes children =
    button
        (class "py-1 px-3 bg-indigo-600 rounded hover:bg-indigo-700"
            :: attributes
        )
        children


viewTodos : Maybe Todo -> List Todo -> Html Msg
viewTodos maybeEditTodo todos =
    div [ class "max-w-2xl mx-auto flex flex-col gap-3" ]
        (List.map (viewTodo maybeEditTodo) todos)


viewTodo : Maybe Todo -> Todo -> Html Msg
viewTodo maybeEditTodo todo =
    let
        notEditingTodo =
            div [ class "px-3 py-2 flex gap-3 items-center bg-gray-800 break-all" ]
                [ input
                    [ type_ "checkbox"
                    , checked todo.isComplete
                    , onCheck (ToggleTodo todo.id)
                    , class "w-5 h-5"
                    ]
                    []
                , div [ class "grow" ] [ text todo.text ]
                , blueButton
                    [ onClick (StartEditingTodo todo.id)
                    , attribute "aria-label" "Edit"
                    ]
                    [ editIcon ]
                , blueButton
                    [ onClick (RemoveTodo todo.id)
                    , attribute "aria-label" "Remove"
                    ]
                    [ removeIcon ]
                ]
    in
    case maybeEditTodo of
        Nothing ->
            notEditingTodo

        Just editTodo ->
            if editTodo.id == todo.id then
                div [ class "p-2 border-2 flex gap-2 items-center" ]
                    [ input
                        [ class "grow p-1 px-2 bg-gray-800 rounded"
                        , value editTodo.text
                        , onInput SetEditingTodoText
                        ]
                        []
                    , blueButton
                        [ onClick SaveEditingTodo
                        , attribute "aria-label" "Save"
                        ]
                        [ saveIcon ]
                    , blueButton
                        [ onClick CancelEditingTodo
                        , attribute "aria-label" "Cancel"
                        ]
                        [ cancelIcon ]
                    ]

            else
                notEditingTodo


svgIcon : String -> Html msg
svgIcon pathAttribute =
    svg
        [ attribute "aria-hidden" "true"
        , viewBox "0 0 24 24"
        , style "height" "24px"
        , style "fill" "currentColor"
        ]
        [ path [ d pathAttribute ] [] ]


saveIcon : Html msg
saveIcon =
    svgIcon "M17 3H5c-1.11 0-2 .9-2 2v14c0 1.1.89 2 2 2h14c1.1 0 2-.9 2-2V7l-4-4zm-5 16c-1.66 0-3-1.34-3-3s1.34-3 3-3 3 1.34 3 3-1.34 3-3 3zm3-10H5V5h10v4z"


editIcon : Html msg
editIcon =
    svgIcon "M3 17.25V21h3.75L17.81 9.94l-3.75-3.75L3 17.25zM20.71 7.04c.39-.39.39-1.02 0-1.41l-2.34-2.34a.9959.9959 0 0 0-1.41 0l-1.83 1.83 3.75 3.75 1.83-1.83z"


removeIcon : Html msg
removeIcon =
    svgIcon "M6 19c0 1.1.9 2 2 2h8c1.1 0 2-.9 2-2V7H6v12zM19 4h-3.5l-1-1h-5l-1 1H5v2h14V4z"


cancelIcon : Html msg
cancelIcon =
    svgIcon "M19 6.41 17.59 5 12 10.59 6.41 5 5 6.41 10.59 12 5 17.59 6.41 19 12 13.41 17.59 19 19 17.59 13.41 12z"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


port saveTodos : List Todo -> Cmd msg


main : Program Json.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
