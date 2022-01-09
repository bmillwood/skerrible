module View exposing (view)

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

    stateDisplay =
      [ case state of
          Model.PreLogin preLogin ->
            Html.map (Ok << Msg.PreLogin) (viewPreLogin preLogin)
          Model.Chatting chatting ->
            Html.map Ok (viewChatting chatting)
      ]
  in
  Html.div [] (errorDisplay ++ stateDisplay)
