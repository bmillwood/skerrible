module View exposing (view)

import Array
import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode

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

tileStyle : List (Html.Attribute msg)
tileStyle =
  [ Attributes.style "width" "1.5em"
  , Attributes.style "height" "1.5em"
  , Attributes.style "text-align" "center"
  ]

tileColor : String
tileColor = "beige"

viewBoard : Model.Board -> Maybe Model.Move -> Html Msg.OkMsg
viewBoard { top, left, squares } proposedMove =
  let
    placedTile rowN colN =
      case proposedMove of
        Nothing -> Nothing
        Just { startRow, startCol, direction, tiles } ->
          case direction of
            Model.MoveRight ->
              if rowN == startRow && colN >= startCol
              then List.head (List.drop (colN - startCol) tiles)
              else Nothing
            Model.MoveDown ->
              if colN == startCol && rowN >= startRow
              then List.head (List.drop (rowN - startRow) tiles)
              else Nothing
    square rowIx colIx sq =
      let
        rowN = rowIx + top
        colN = colIx + left
        placed = placedTile rowN colN
        isJust = Maybe.withDefault False << Maybe.map (always True)
        tileHere = isJust sq.tile || isJust placed
        bgColor =
          if tileHere
          then tileColor
          else
            case sq.wordMult of
              3 -> "#f77"
              2 -> "#fcc"
              _ ->
                case sq.letterMult of
                  3 -> "#88f"
                  2 -> "#bbf"
                  _ -> "#ccc"
        redIfMove =
          case placed of
            Nothing -> []
            Just _ -> [ Attributes.style "color" "#f00" ]
        directionIfHere =
          case proposedMove of
            Nothing -> Nothing
            Just { startRow, startCol, direction } ->
              if startRow == rowN && startCol == colN
              then Just direction
              else Nothing
        newMove direction =
          { startRow = rowN, startCol = colN, direction = direction, tiles = [] }
        attributes =
          [ [ Attributes.style "background-color" bgColor
            , Events.onClick (
                case proposedMove of
                  Nothing -> Msg.ProposeMove (Just (newMove Model.MoveRight))
                  Just { startRow, startCol, direction, tiles } ->
                    if not (List.isEmpty tiles)
                    then Msg.DoNothing
                    else
                      case directionIfHere of
                        Nothing -> Msg.ProposeMove (Just (newMove Model.MoveRight))
                        Just Model.MoveRight -> Msg.ProposeMove (Just (newMove Model.MoveDown))
                        Just Model.MoveDown -> Msg.ProposeMove Nothing
              )
            ]
          , redIfMove
          , tileStyle
          ] |> List.concat
        char =
          case placed of
            Nothing ->
              case sq.tile of
                Just t -> t.char
                Nothing ->
                  case directionIfHere of
                    Nothing -> ' '
                    Just Model.MoveRight -> '→'
                    Just Model.MoveDown -> '↓'
            Just tile -> tile.char
      in
      Html.td attributes [ Html.text (String.fromChar char) ]
    rowNumCell n = Html.th [ Attributes.scope "row" ] [ Html.text (String.fromInt (n + 1)) ]
    tableRow rowIx row =
      Html.tr
        []
        (rowNumCell rowIx :: List.indexedMap (square rowIx) (Array.toList row))
    boardWidth = Array.foldl max 0 (Array.map Array.length squares)
    colLetter i = String.fromChar (Char.fromCode (Char.toCode 'A' + i))
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

    stateDisplay =
      [ case state of
          Model.PreLogin preLogin ->
            Html.map (Ok << Msg.PreLogin) (viewPreLogin preLogin)
          Model.InGame { chat, game } ->
            Html.map Ok (
                Html.div
                  []
                  [ viewBoard game.board game.proposedMove
                  , viewRack game.rack
                  , viewChatting chat
                  ]
              )
      ]
  in
  Html.div [] (errorDisplay ++ stateDisplay)
