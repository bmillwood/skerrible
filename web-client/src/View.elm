module View exposing (view)

import Array
import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode

import Board exposing (Board)
import Model
import Move exposing (Move)
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

viewBoard
  : { board : Board, proposedMove : Maybe Move, squareError : Maybe (Int, Int) }
  -> Html Msg.OkMsg
viewBoard { board, proposedMove, squareError } =
  let
    { top, left, squares } = board
    square rowIx colIx sq =
      let
        rowN = rowIx + top
        colN = colIx + left
        placed = proposedMove |> Maybe.andThen (Move.tileAtPos rowN colN)
        isJust = Maybe.withDefault False << Maybe.map (always True)
        tileHere = isJust sq.tile || isJust placed
        errorHere =
          case squareError of
            Just (i, j) -> i == rowN && j == colN
            Nothing -> False
        bgColor =
          if errorHere
          then "red"
          else if tileHere
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
                  Nothing -> Msg.ProposeMove (Just (newMove Move.Right))
                  Just { startRow, startCol, direction, tiles } ->
                    if not (List.isEmpty tiles)
                    then Msg.DoNothing
                    else
                      case directionIfHere of
                        Nothing -> Msg.ProposeMove (Just (newMove Move.Right))
                        Just Move.Right -> Msg.ProposeMove (Just (newMove Move.Down))
                        Just Move.Down -> Msg.ProposeMove Nothing
              )
            ]
          , redIfMove
          , tileStyle
          ] |> List.concat
        char =
          case placed of
            Just (Move.PlaceTile tile) -> tile.char
            _ ->
              case sq.tile of
                Just t -> t.char
                Nothing ->
                  case directionIfHere of
                    Nothing -> ' '
                    Just Move.Right -> '→'
                    Just Move.Down -> '↓'
      in
      Html.td attributes [ Html.text (String.fromChar char) ]
    rowNumCell n = Html.th [ Attributes.scope "row" ] [ Html.text (String.fromInt (n + 1)) ]
    tableRow rowIx row =
      Html.tr
        []
        (rowNumCell rowIx :: List.indexedMap (square rowIx) (Array.toList row))
    colLetter i = String.fromChar (Char.fromCode (Char.toCode 'A' + i))
    colHeaders =
      List.indexedMap
        (\i () -> Html.th [ Attributes.scope "col" ] [ Html.text (colLetter i) ])
        (List.repeat (Board.width board) ())
    headerRow = Html.tr [] (Html.td [] [] :: colHeaders)
  in
  Html.table
    []
    (headerRow :: List.indexedMap tableRow (Array.toList squares))

viewError : Maybe Move.Error -> Html Msg.OkMsg
viewError error =
  let
    text =
      case error of
        Nothing -> ""
        Just Move.NotPlaying -> "The game is not in progress"
        Just Move.NotYourTurn -> "It's not your turn!"
        Just Move.OffBoard -> "Your move starts or ends off the edge of the board."
        Just Move.TilesDoNotMatchBoard -> "The tiles you provided don't match the ones on the board."
        Just Move.NoPlacedTiles -> "Your move doesn't use any tiles from your rack."
        Just (Move.YouDoNotHave _) -> "You don't have the letters necessary for that move."
        Just Move.DoesNotConnect -> "Your move doesn't connect with existing tiles."
        Just (Move.NotAWord _) -> "At least one of the words you made doesn't exist."
  in
  Html.div
    [ Events.onClick Msg.ClearMoveError
    , Attributes.style "color" "red"
    ]
    [ Html.text text ]

viewRack : { rack : Board.Rack, rackError : Bool } -> Html Msg.OkMsg
viewRack { rack, rackError } =
  let
    attributes =
      [ [ Attributes.style "background-color" tileColor ]
      , tileStyle
      ] |> List.concat
    rackTile tile =
      Html.td attributes [ Html.text (String.fromChar tile.char) ]
    spaceTd =
      -- it seems like the cells of a table are stretched to the width of the
      -- table, but we want ours to be fixed size, so we include this one to mop
      -- up the "unused" width.
      Html.td [] []
  in
  Html.table
    [ Attributes.style "border" "1px solid black"
    , Attributes.style "background-color" (if rackError then "red" else "green")
    , -- since tds are 1.5em, it seems like this should be 1.5 * 7 = 10.5em, but
      -- it seems like we need to compensate for padding and margin as well
      -- plus the "real" racks have a bit of extra space in them anyway
      Attributes.style "width" "14em"
    ]
    [ Html.tr [] (List.map rackTile rack ++ [ spaceTd ]) ]

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
                  [ viewBoard
                      { board = game.board
                      , proposedMove = game.proposedMove
                      , squareError =
                          game.transientError
                          |> Maybe.andThen (\err ->
                            case err of
                              Model.RackError -> Nothing
                              Model.SquareError i j -> Just (i, j))
                      }
                  , viewError game.moveError
                  , viewRack
                      { rack = game.rack
                      , rackError = game.transientError == Just Model.RackError
                      }
                  , viewChatting chat
                  ]
              )
      ]
  in
  Html.div [] (errorDisplay ++ stateDisplay)
