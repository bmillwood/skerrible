module View exposing (view)

import Array
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events

import Model
import Msg exposing (Msg)

viewPreLogin : Model.PreLoginState -> Html Msg.LoginFormMsg
viewPreLogin { loginState, loginForm } =
  let
    endpointInput =
      Html.input
        [ Attributes.type_ "text"
        , Attributes.name "endpoint"
        , Attributes.value loginForm.endpoint
        , Events.onInput (\input -> Msg.Update { loginForm | endpoint = input })
        , Attributes.disabled (loginState == Model.Waiting)
        ]
        []

    usernameInput =
      Html.input
        [ Attributes.type_ "text"
        , Attributes.name "username"
        , Attributes.value loginForm.username
        , Events.onInput (\input -> Msg.Update { loginForm | username = input })
        , Attributes.disabled (loginState == Model.Waiting)
        ]
        []

    submitButton =
      Html.input
        [ Attributes.type_ "submit"
        , Attributes.name "login"
        , Attributes.value "Login"
        ]
        []
  in
  Html.form
    [ Events.onSubmit Msg.Submit ]
    [ Html.p [] [ Html.text "Server: ", endpointInput ]
    , Html.p [] [ Html.text "Username: ", usernameInput ]
    , Html.p [] [ submitButton ]
    ]

tileCell : { backgroundColor : String } -> Char -> Html msg
tileCell { backgroundColor } char =
  Html.td
    [ Attributes.style "background-color" backgroundColor
    , Attributes.style "width" "1.5em"
    , Attributes.style "height" "1.5em"
    , Attributes.style "text-align" "center"
    ]
    [ Html.text (String.fromChar char) ]

tileStyle : List (Html.Attribute msg)
tileStyle =
  [ Attributes.style "width" "1.5em"
  , Attributes.style "height" "1.5em"
  , Attributes.style "text-align" "center"
  ]

tileColor : String
tileColor = "beige"

viewBoard : Model.Board -> Html Msg.OkMsg
viewBoard { topLeft, squares } =
  let
    square sq =
      let
        bgColor =
          case sq.tile of
            Just _ -> tileColor
            Nothing ->
              case sq.wordMult of
                3 -> "#f77"
                2 -> "#fcc"
                _ ->
                  case sq.letterMult of
                    3 -> "#88f"
                    2 -> "#bbf"
                    _ -> "#ccc"
        attributes =
          [ [ Attributes.style "background-color" bgColor ]
          , tileStyle
          ] |> List.concat
        char = Maybe.withDefault ' ' (Maybe.map .char sq.tile)
      in
      Html.td attributes [ Html.text (String.fromChar char) ]
    rowNumCell n = Html.th [ Attributes.scope "row" ] [ Html.text (String.fromInt (n + 1)) ]
    tableRow rowNumber row = Html.tr [] (rowNumCell rowNumber :: List.map square (Array.toList row))
    boardWidth = Array.foldl max 0 (Array.map Array.length squares)
    colLetter n = String.fromChar (Char.fromCode (Char.toCode 'A' + n))
    colHeaders =
      List.indexedMap
        (\i () -> Html.th [ Attributes.scope "col" ] [ Html.text (colLetter i) ])
        (List.repeat boardWidth ())
    headerRow = Html.tr [] (Html.td [] [] :: colHeaders)
  in
  Html.table
    []
    (headerRow :: List.indexedMap tableRow (Array.toList squares))

viewRack : Model.Rack -> Html Msg.OkMsg
viewRack rack =
  let
    attributes =
      [ [ Attributes.style "background-color" tileColor ]
      , tileStyle
      ] |> List.concat
    rackTile tile =
      Html.td attributes [ Html.text (String.fromChar tile.char) ]
  in
  Html.table
    [ Attributes.style "border" "1px solid black"
    , Attributes.style "background-color" "green"
    ]
    [ Html.tr [] (List.map rackTile rack) ]

viewChatting : Model.ChattingState -> Html Msg.OkMsg
viewChatting { folks, me, messageEntry, history } =
  let
    inputRow =
      Html.tr
        []
        [ Html.td
            []
            [ Html.text me ]
        , Html.td
            []
            [ Html.form
                [ Events.onSubmit (Msg.SendMessage messageEntry) ]
                [ Html.input
                    [ Attributes.type_ "text"
                    , Attributes.placeholder "chat"
                    , Attributes.value messageEntry
                    , Events.onInput Msg.ComposeMessage
                    ]
                    []
                ]
            ]
        ]

    historyRow item =
      let
        (username, content) =
          case item of
            Model.Joined joiner -> (joiner, Html.text "joined")
            Model.Left leaver -> (leaver, Html.text "left")
            Model.Chatted { sender, message } ->
              ( sender
              , Html.text (": " ++ message)
              )
      in
      Html.tr
        []
        [ Html.td [] [ Html.text username ]
        , Html.td [] [ content ]
        ]
  in
  Html.table []
    (inputRow :: List.map historyRow history)

view : Model.Model -> Html Msg
view { error, state } =
  let
    errorDisplay =
      case error of
        Nothing ->
          []
        Just errorMsg ->
          [ Html.pre
              [ Attributes.style "background-color" "hsl(0,0.5,0.9)"
              , Attributes.style "padding" "1em"
              , Attributes.style "border" "1px solid hsl(0,1,0.5)"
              ]
              [ Html.text errorMsg ] ]

    sendTestMove =
      Msg.SendMove
        { startPos = (8, 5)
        , direction = Model.MoveRight
        , tiles =
            [ { char = 'A', score = 1 }
            , { char = 'M', score = 2 }
            , { char = 'B', score = 3 }
            , { char = 'E', score = 1 }
            , { char = 'R', score = 1 }
            ]
        }

    stateDisplay =
      [ case state of
          Model.PreLogin preLogin ->
            Html.map (Ok << Msg.PreLogin) (viewPreLogin preLogin)
          Model.InGame { chat, board, rack } ->
            Html.map Ok (
                Html.div
                  []
                  [ viewBoard board
                  , Html.button
                      [ Events.onClick sendTestMove ]
                      [ Html.text "Test move" ]
                  , viewRack rack
                  , viewChatting chat
                  ]
              )
      ]
  in
  Html.div [] (errorDisplay ++ stateDisplay)
