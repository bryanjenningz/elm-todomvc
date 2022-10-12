module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes exposing (checked, class, classList, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Random


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


type Filter
    = FilterAll
    | FilterIncomplete
    | FilterComplete


init : () -> ( Model, Cmd Msg )
init () =
    ( { newTodoText = ""
      , newTodoId = Nothing
      , editingTodo = Nothing
      , todos = []
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
                    in
                    ( { model
                        | todos = model.todos ++ [ newTodo ]
                        , newTodoText = ""
                        , newTodoId = Nothing
                      }
                    , generateNewTodoId
                    )

        ToggleTodo todoId isComplete ->
            ( { model
                | todos =
                    updateTodo todoId
                        (\todo -> { todo | isComplete = isComplete })
                        model.todos
              }
            , Cmd.none
            )

        RemoveTodo todoId ->
            ( { model | todos = removeTodo todoId model.todos }, Cmd.none )

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
                    ( { model
                        | todos =
                            updateTodo editingTodo.id
                                (\_ -> editingTodo)
                                model.todos
                        , editingTodo = Nothing
                      }
                    , Cmd.none
                    )

        ToggleAllTodos isComplete ->
            ( { model
                | todos =
                    List.map (\todo -> { todo | isComplete = isComplete })
                        model.todos
              }
            , Cmd.none
            )

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
            [ div [ class "flex max-w-xl md:w-2/3 mx-auto mb-3" ]
                [ input
                    [ class "grow p-1 px-2 bg-gray-800 rounded"
                    , onInput SetNewTodoText
                    , value model.newTodoText
                    , placeholder "Todo text"
                    ]
                    []
                , div [ class "w-1/4 flex" ] [ viewButton AddNewTodo "Add" ]
                ]
            , div [ class "flex gap-2 justify-center" ]
                [ viewFilter model.filter FilterAll "All"
                , viewFilter model.filter FilterIncomplete "Incomplete"
                , viewFilter model.filter FilterComplete "Complete"
                ]
            , label [ class "max-w-2xl mx-auto flex gap-3 items-center" ]
                [ input
                    [ type_ "checkbox"
                    , checked (allTodosAreComplete model)
                    , onCheck ToggleAllTodos
                    , class "w-5 h-5"
                    ]
                    []
                , span [] [ text "Check all" ]
                ]
            , viewTodos model.editingTodo filteredTodos
            ]
        ]


viewFilter : Filter -> Filter -> String -> Html Msg
viewFilter activeFilter filter filterText =
    button
        [ classList
            [ ( "text-cyan-400", filter == activeFilter )
            ]
        , class "px-3 py-2 hover:bg-gray-800 rounded"
        , onClick (SetFilter filter)
        ]
        [ text filterText ]


viewButton : msg -> String -> Html msg
viewButton onClickMsg buttonText =
    button
        [ class "grow py-1 px-3 bg-indigo-600 rounded hover:bg-indigo-700"
        , onClick onClickMsg
        ]
        [ text buttonText ]


viewTodos : Maybe Todo -> List Todo -> Html Msg
viewTodos maybeEditTodo todos =
    div [ class "max-w-2xl mx-auto flex flex-col gap-3 mt-3" ]
        (List.map (viewTodo maybeEditTodo) todos)


viewTodo : Maybe Todo -> Todo -> Html Msg
viewTodo maybeEditTodo todo =
    let
        notEditingTodo =
            div [ class "px-3 py-2 flex gap-3 items-center bg-gray-800" ]
                [ input
                    [ type_ "checkbox"
                    , checked todo.isComplete
                    , onCheck (ToggleTodo todo.id)
                    , class "w-5 h-5"
                    ]
                    []
                , div [ class "grow" ] [ text todo.text ]
                , div [] [ viewButton (StartEditingTodo todo.id) "✎" ]
                , div [] [ viewButton (RemoveTodo todo.id) "x" ]
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
                    , div [] [ viewButton SaveEditingTodo "Save" ]
                    , div [] [ viewButton CancelEditingTodo "Cancel" ]
                    ]

            else
                notEditingTodo


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
