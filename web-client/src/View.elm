module View exposing (view)

import Array
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode

import Board exposing (Board)
import DictTile exposing (DictTile)
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
  [ Attributes.style "width" "1.8em"
  , Attributes.style "height" "1.8em"
  , Attributes.style "text-align" "center"
  ]

tileColor : String
tileColor = "beige"

viewTile
  :  Board.Tile
  -> DictTile Board.TileData
  -> { partOfMove : Bool, error : Bool }
  -> Html msg
viewTile tile tileData { partOfMove, error } =
  let
    scoreDisplay =
      case DictTile.get tile tileData of
        Nothing -> "?"
        Just { score } -> String.fromInt score
    attributes =
      [ Attributes.style "color" (if partOfMove then "red" else "black")
      , Attributes.style "background-color" (if error then "red" else tileColor)
      , Attributes.style "position" "relative"
      ] ++ tileStyle
  in
  Html.td
    attributes
    [ Html.text (String.fromChar (Board.tileToChar tile))
    , Html.span
        [ Attributes.style "position" "absolute"
        , Attributes.style "font-size" "50%"
        , Attributes.style "bottom" "0.2em"
        , Attributes.style "right" "0.2em"
        ]
        [ Html.text scoreDisplay ]
    ]

viewBoard
  :  { board : Board
     , tileData : DictTile Board.TileData
     , proposedMove : Maybe Move
     , transientError : Maybe Model.TransientError
     }
  -> Html Msg.OkMsg
viewBoard { board, tileData, proposedMove, transientError } =
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
          case transientError of
            Just (Model.SquareError i j) -> i == rowN && j == colN
            _ -> False
        bgColor =
          case sq.wordMult of
            3 -> "#f77"
            2 -> "#fcc"
            _ ->
              case sq.letterMult of
                3 -> "#88f"
                2 -> "#bbf"
                _ -> "#ccc"
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
          , tileStyle
          ] |> List.concat
        tile =
          case placed of
            Just (Move.PlaceTile t) -> Just t
            _ ->
              case sq.tile of
                Just t -> Just t
                Nothing -> Nothing
        char =
          case directionIfHere of
            Nothing -> ' '
            Just Move.Right -> '→'
            Just Move.Down -> '↓'
      in
      case tile of
        Just t -> viewTile t tileData { partOfMove = isJust placed, error = errorHere }
        Nothing -> Html.td attributes [ Html.text (String.fromChar char) ]
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
    boardErrorAttributes =
      case (transientError, proposedMove) of
        (Just Model.BoardError, Just { direction }) ->
          let
            which =
              case direction of
                Move.Right -> "border-right"
                Move.Down -> "border-bottom"
          in
          [ Attributes.style which "2px solid red" ]
        _ -> []
  in
  Html.table
    (boardErrorAttributes ++ [ Attributes.style "float" "left" ])
    (headerRow :: List.indexedMap tableRow (Array.toList squares))

viewScores : Dict String Int -> Html msg
viewScores scores =
  Html.table [] (
    Dict.toList scores
    |> List.sortBy (\(_, score) -> -score)
    |> List.map (\(person, score) ->
          Html.tr
            []
            [ Html.td [] [ Html.text person ]
            , Html.td [] [ Html.text (String.fromInt score) ]
            ]
        )
    |> List.append [
          Html.tr
            []
            [ Html.th [] [ Html.text "Username" ]
            , Html.th [] [ Html.text "Score" ]
            ]
        ]
  )

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
        Just Move.FirstMoveNotInCentre -> "The first move must go through the centre tile."
        Just Move.NoMultiletterWordsMade -> "At least one of the words you make must have at least two letters."
        Just Move.DoesNotConnect -> "Your move doesn't connect with existing tiles."
        Just (Move.NotAWord _) -> "At least one of the words you made doesn't exist."
  in
  Html.div
    [ Events.onClick Msg.ClearMoveError
    , Attributes.style "color" "red"
    ]
    [ Html.text text ]

viewRack
  :  { rack : Board.Rack
     , tileData : DictTile Board.TileData
     , rackError : Bool
     }
  -> Html Msg.OkMsg

viewRack { rack, tileData, rackError } =
  let
    rackTile tile =
      viewTile tile tileData { partOfMove = False, error = False }
    spaceTd =
      -- it seems like the cells of a table are stretched to the width of the
      -- table, but we want ours to be fixed size, so we include this one to mop
      -- up the "unused" width.
      Html.td [] []
  in
  Html.table
    [ Attributes.style "border" "1px solid black"
    , Attributes.style "background-color" (if rackError then "red" else "green")
    , -- since tds are 1.8em, it seems like this should be 1.8 * 7 = 12.6em, but
      -- it seems like we need to compensate for padding and margin as well
      -- plus the "real" racks have a bit of extra space in them anyway
      Attributes.style "width" "17em"
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
            Model.Joined joiner -> (joiner, [ Html.text "joined" ])
            Model.Left leaver -> (leaver, [ Html.text "left" ])
            Model.Chatted { sender, message } ->
              ( sender
              , [ Html.text (": " ++ message) ]
              )
            Model.PlayerMoved { madeBy, words, score } ->
              ( madeBy
              , [ [ Html.text "played " ]
                , List.concatMap
                    (\word -> [ Html.text ", ", Html.text word ])
                    words
                  |> List.drop 1
                , [ Html.text ", for "
                  , Html.text (String.fromInt score)
                  , Html.text " points"
                  ]
                ] |> List.concat
              )
      in
      Html.tr
        []
        [ Html.td [] [ Html.text username ]
        , Html.td [] content
        ]
  in
  Html.table []
    (inputRow :: List.map historyRow history)

view : Model.Model -> Html Msg
view { error, state } =
  let
    title =
      [ Html.text "skerrible | "
      , Html.a
          [ Attributes.href "https://github.com/bmillwood/skerrible" ]
          [ Html.text "github" ]
      , Html.hr [] []
      ]

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
              [ Html.text errorMsg
              , Html.text " "
              , Html.a
                  [ Attributes.href "#"
                  , Events.onClick (Ok Msg.ClearError)
                  ]
                  [ Html.text "clear" ]
              ]
          ]

    stateDisplay =
      [ case state of
          Model.PreLogin preLogin ->
            Html.map (Ok << Msg.PreLogin) (viewPreLogin preLogin)
          Model.InGame { chat, game } ->
            let
              remainingRack =
                case game.proposedMove of
                  Nothing -> game.rack
                  Just move -> Move.remainingRack move game.rack
            in
            Html.map Ok (
                Html.div
                  []
                  [ viewBoard
                      { board = game.board
                      , tileData = game.tileData
                      , proposedMove = game.proposedMove
                      , transientError = game.transientError
                      }
                  , viewScores game.scores
                  , Html.hr [ Attributes.style "clear" "both" ] []
                  , viewError game.moveError
                  , viewRack
                      { rack = remainingRack
                      , tileData = game.tileData
                      , rackError = game.transientError == Just Model.RackError
                      }
                  , Html.hr [ Attributes.style "clear" "both" ] []
                  , viewChatting chat
                  ]
              )
      ]
  in
  Html.div
    []
    (title ++ errorDisplay ++ stateDisplay)
