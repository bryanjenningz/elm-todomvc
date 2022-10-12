module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Model =
    { counts : List Int }


init : () -> ( Model, Cmd Msg )
init () =
    ( { counts = [ 0 ] }, Cmd.none )


type Msg
    = NewCount
    | Increment Int
    | Decrement Int
    | Reset Int
    | Remove Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCount ->
            ( { model | counts = model.counts ++ [ 0 ] }, Cmd.none )

        Increment index ->
            ( { model | counts = updateAt index (\x -> x + 1) model.counts }, Cmd.none )

        Decrement index ->
            ( { model | counts = updateAt index (\x -> x - 1) model.counts }, Cmd.none )

        Reset index ->
            ( { model | counts = updateAt index (\_ -> 0) model.counts }, Cmd.none )

        Remove index ->
            ( { model | counts = removeAt index model.counts }, Cmd.none )


updateAt : Int -> (a -> a) -> List a -> List a
updateAt index updater list =
    List.indexedMap
        (\i x ->
            if i == index then
                updater x

            else
                x
        )
        list


removeAt : Int -> List a -> List a
removeAt index list =
    List.indexedMap (\i x -> ( i, x )) list
        |> List.filterMap
            (\( i, x ) ->
                if i == index then
                    Nothing

                else
                    Just x
            )


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-black text-white" ]
        [ div
            [ class "container mx-auto" ]
            [ div [ class "flex w-full md:w-1/2 mx-auto" ] [ viewButton NewCount "New count" ]
            , div [ class "flex flex-col items-center" ]
                (List.indexedMap viewCount model.counts)
            ]
        ]


viewCount : Int -> Int -> Html Msg
viewCount index count =
    div [ class "w-1/3 flex flex-col items-center" ]
        [ div [ class "text-xl my-2" ]
            [ text (String.fromInt count) ]
        , div [ class "flex gap-2 w-full" ]
            [ viewButton (Increment index) "+"
            , viewButton (Decrement index) "-"
            , viewButton (Reset index) "o"
            , viewButton (Remove index) "x"
            ]
        ]


viewButton : msg -> String -> Html msg
viewButton onClickMsg buttonText =
    button
        [ class "grow py-1 px-3 bg-indigo-600 rounded hover:bg-indigo-700"
        , onClick onClickMsg
        ]
        [ text buttonText ]


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
