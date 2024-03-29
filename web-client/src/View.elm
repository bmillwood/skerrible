module View exposing (view)

import Array
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode
import Set exposing (Set)

import Board exposing (Board)
import DictTile exposing (DictTile)
import Model
import Move exposing (Move)
import Msg exposing (Msg)

type alias Radio a =
  { id : String
  , checked : Bool
  , onChecked : a
  , contents : List (Html a)
  }

radios
  :  { name : String, disabled : Bool }
  -> List (Radio a)
  -> Html a
radios { name, disabled } items =
  let
    radio { id, checked, onChecked, contents } =
      Html.li
        []
        [ Html.input
            [ Attributes.type_ "radio"
            , Attributes.id id
            , Attributes.name name
            , Attributes.checked checked
            , Attributes.disabled disabled
            , Events.onCheck (\_ -> onChecked)
            ]
            []
        , Html.label
            [ Attributes.for id ]
            contents
        ]
  in
  Html.ul
    [ Attributes.style "list-style-type" "none" ]
    (List.map radio items)

viewPreLogin : Model.PreLoginState -> Html Msg
viewPreLogin { loginState, loginForm } =
  let
    { endpoint, username, roomCode, roomAction, roomSettings } = loginForm

    endpointInput =
      Html.input
        [ Attributes.type_ "text"
        , Attributes.value endpoint
        , Events.onInput (\input -> Msg.Update { loginForm | endpoint = input })
        , Attributes.disabled (loginState == Model.Waiting)
        ]
        []

    usernameInput =
      Html.input
        [ Attributes.type_ "text"
        , Attributes.value username
        , Events.onInput (\input -> Msg.Update { loginForm | username = input })
        , Attributes.disabled (loginState == Model.Waiting)
        ]
        []

    roomCodeInput =
      Html.input
        [ Attributes.type_ "text"
        , Attributes.value roomCode
        , Attributes.placeholder "code"
        , Attributes.disabled (roomAction /= Model.JoinRoom)
        , Events.onInput (\newCode -> Msg.Update { loginForm | roomCode = newCode })
        ]
        []

    noBoardMultipliers =
      let
        id = "noBoardMultipliers"
      in
      Html.li
        []
        [ Html.input
            [ Attributes.type_ "checkbox"
            , Attributes.id id
            , Attributes.checked roomSettings.noBoardMultipliers
            , Attributes.disabled (roomAction /= Model.MakeNewRoom)
            , Events.onCheck (\newChecked ->
                  { roomSettings | noBoardMultipliers = newChecked }
                )
            ]
            []
        , Html.label
            [ Attributes.for id ]
            [ Html.text "Disable multiplier squares" ]
        ]

    turnEnforcement =
      Html.li
        []
        [ Html.text "Turn order enforcement:"
        , radios
          { name = "turnEnforcement"
          , disabled = roomAction == Model.JoinRoom
          }
          [ { id = "none"
            , checked = roomSettings.turnEnforcement == Model.NoEnforcement
            , onChecked = { roomSettings | turnEnforcement = Model.NoEnforcement }
            , contents = [ Html.text "None (anyone can move at any time)" ]
            }
          , { id = "letPlayersChoose"
            , checked = roomSettings.turnEnforcement == Model.LetPlayersChoose
            , onChecked = { roomSettings | turnEnforcement = Model.LetPlayersChoose }
            , contents = [ Html.text "Enforce the order players first play in" ]
            }
          ]
        ]

    roomSpec =
      radios
        { name = "roomSpec", disabled = False }
        [ { id = "joinRoom"
          , checked = roomAction == Model.JoinRoom
          , onChecked = Msg.Update { loginForm | roomAction = Model.JoinRoom }
          , contents =
              [ Html.text "Join room: "
              , roomCodeInput
              ]
          }
        , { id = "makeNewRoom"
          , checked = roomAction == Model.MakeNewRoom
          , onChecked = Msg.Update { loginForm | roomAction = Model.MakeNewRoom }
          , contents =
              [ Html.text "Make new room:"
              , Html.ul
                  []
                  [ noBoardMultipliers
                  , turnEnforcement
                  ]
                |> Html.map (\settings ->
                      Msg.Update { loginForm | roomSettings = settings }
                    )
              ]
          }
        ]

    cannotSubmit = String.isEmpty loginForm.username
    submitButton =
      Html.input
        [ Attributes.type_ "submit"
        , Attributes.name "login"
        , Attributes.value "Login"
        , Attributes.disabled cannotSubmit
        ]
        []

    preLoginMsg msg = [Msg.PreLogin msg]
  in
  Html.form
    [ Events.onSubmit (
          if cannotSubmit
          then []
          else preLoginMsg Msg.Submit
        )
    ]
    [ Html.p []
        [ Html.text "Username: "
        , Html.map preLoginMsg usernameInput
        ]
    , Html.map preLoginMsg roomSpec
    , Html.p []
        [ Html.text "Server: "
        , Html.map preLoginMsg endpointInput
        , Html.text " (default is usually correct)"
        ]
    , Html.p [] [ submitButton ]
    ]

squareStyle =
  [ Attributes.style "width" "1.8em"
  , Attributes.style "height" "1.8em"
  , Attributes.style "text-align" "center"
  ]

viewTile
  :  Board.Tile
  -> DictTile Board.TileData
  -> { partOfMove : Bool, error : Bool }
  -> Html Msg
viewTile tile tileData { partOfMove, error } =
  let
    scoreDisplay =
      case DictTile.get tile tileData of
        Nothing -> "?"
        Just { score } -> String.fromInt score
    attributes =
      [ Attributes.style "color" (if partOfMove then "red" else "black")
      , Attributes.style "background-color" (if error then "red" else "beige")
      , Attributes.style "position" "relative"
      , Events.onMouseDown [Msg.InRoom (Msg.UpdateProposal (Msg.ProposeTile tile))]
      ] ++ squareStyle
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
     , gameOver : Bool
     }
  -> Html Msg
viewBoard { board, tileData, proposedMove, transientError, gameOver } =
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
        (bgColor, description) =
          case sq.wordMult of
            3 -> ("#f77", Just "triple word")
            2 -> ("#fcc", Just "double word")
            _ ->
              case sq.letterMult of
                3 -> ("#88f", Just "triple letter")
                2 -> ("#bbf", Just "double letter")
                _ -> ("#ccc", Nothing)
        title =
          case description of
            Nothing -> []
            Just descr -> [ Attributes.title descr ]
        directionIfHere =
          case proposedMove of
            Nothing -> Nothing
            Just { startRow, startCol, direction } ->
              if startRow == rowN && startCol == colN
              then Just direction
              else Nothing
        newMove direction =
          { startRow = rowN, startCol = colN, direction = direction, tiles = [] }
        proposeMove move = [Msg.InRoom (Msg.Propose (Just (Move.ProposeMove move)))]
        attributes =
          [ [ Attributes.style "background-color" bgColor
            , Events.onClick (
                if gameOver
                then []
                else case proposedMove of
                  Nothing -> proposeMove (newMove Move.Right)
                  Just { startRow, startCol, direction, tiles } ->
                    if not (List.isEmpty tiles)
                    then []
                    else
                      case directionIfHere of
                        Nothing -> proposeMove (newMove Move.Right)
                        Just Move.Right -> proposeMove (newMove Move.Down)
                        Just Move.Down -> [Msg.InRoom (Msg.Propose Nothing)]
              )
            ]
          , title
          , squareStyle
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
        Just t ->
          viewTile t tileData
            { partOfMove = isJust placed, error = errorHere }
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

viewPlayers : Dict String Model.PlayerData -> Html msg
viewPlayers players =
  let
    allCanMove = List.all .canMove (Dict.values players)
  in
  Html.table [] (
    Dict.toList players
    |> List.sortBy (\(_, p) -> -p.score)
    |> List.map (\(person, { score, canMove }) ->
          Html.tr
            (if canMove && not allCanMove
              then [ Attributes.style "background-color" "yellow" ]
              else []
            )
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

viewError : Maybe Move.Error -> Html Msg
viewError error =
  let
    text =
      case error of
        Nothing -> ""
        Just Move.YouAreNotPlaying -> "You are not a player in this game."
        Just Move.GameIsOver -> "The game has already ended."
        Just Move.NotYourTurn -> "It's not your turn!"
        Just Move.OffBoard -> "Your move starts or ends off the edge of the board."
        Just Move.TilesDoNotMatchBoard ->
          "The tiles you provided don't match the ones on the board."
        Just Move.NoPlacedTiles -> "Your move doesn't use any tiles from your rack."
        Just (Move.YouDoNotHave _) ->
          "You don't have the letters necessary for that move."
        Just Move.FirstMoveNotInCentre ->
          "The first move must go through the centre tile."
        Just Move.NoMultiletterWordsMade ->
          "Your move doesn't create a word of at least two letters."
        Just Move.DoesNotConnect -> "Your move doesn't connect with existing tiles."
        Just (Move.NotAWord _) -> "At least one of the words you made doesn't exist."
        Just Move.NotEnoughTilesToExchange ->
          "There aren't enough tiles in the bag to exchange that many."
  in
  Html.div
    [ Events.onClick [Msg.InRoom Msg.ClearMoveError]
    , Attributes.style "color" "red"
    ]
    [ Html.text text ]

viewRack
  :  { rack : Board.Rack
     , tileData : DictTile Board.TileData
     , proposedExchange : Maybe (List Board.Tile)
     , gameOver : Bool
     , rackError : Bool
     }
  -> Html Msg
viewRack { rack, tileData, proposedExchange, gameOver, rackError } =
  let
    rackTile tile =
      viewTile tile tileData { partOfMove = False, error = False }
    spaceTd =
      -- it seems like the cells of a table are stretched to the width of the
      -- table, but we want ours to be fixed size, so we include this one to mop
      -- up the "unused" width.
      Html.td [] []
  in
  Html.div
    []
    [ Html.p
        []
        [ Html.table
            [ Attributes.style "border" "1px solid black"
            , Attributes.style "background-color" (if rackError then "red" else "green")
            , -- since tds are 1.8em, it seems like this should be 1.8 * 7 = 12.6em,
              -- but it seems like we need to compensate for padding and margin as well
              -- plus the "real" racks have a bit of extra space in them anyway
              Attributes.style "width" "17em"
            , Attributes.style "min-height" "1.8em"
            ]
            [ Html.tr [] (List.map rackTile rack ++ [ spaceTd ]) ]
        , Html.button
            [ Events.onClick [Msg.InRoom (Msg.UpdateProposal Msg.UnproposeLast)]
            , Attributes.disabled gameOver
            ]
            [ Html.text "\u{232b} Backspace" ]
        , Html.button
            [ Events.onClick [Msg.InRoom (Msg.UpdateProposal Msg.SubmitProposal)]
            , Attributes.disabled gameOver
            ]
            [ Html.text "\u{21b5} Submit" ]
        ]
    , Html.p
        []
        [ Html.button
            [ Events.onClick [Msg.InRoom (Msg.ShuffleRack Nothing)] ]
            [ Html.text "\u{1f500} Rearrange" ]
        , let
            id = "exchangeButton"
          in
          Html.button
            [ Attributes.id id
            , Events.onClick (
                [ Msg.InRoom (Msg.Propose (Just (Move.ProposeExchange [])))
                , -- if I don't do this the keypresses don't reach the global handler
                  Msg.Global (Msg.BlurById id)
                ]
              )
            , Attributes.disabled
              <| case proposedExchange of
                Nothing -> gameOver
                Just _ -> True
            ]
            [ Html.text "\u{1f5d1} "
            , Html.text
              <| case proposedExchange of
                Nothing -> "Exchange"
                Just tiles ->
                  "Exchanging " ++ String.fromList (List.map Board.tileToChar tiles)
            ]
        , Html.button
            [ Events.onClick [Msg.InRoom Msg.SendPass]
            , Attributes.disabled gameOver
            ]
            [ Html.text "\u{1f937} Pass" ]
        ]
    ]

viewChatting : Model.ChattingState -> { spectators : Set String } -> Html Msg
viewChatting { me, messageEntry, history } { spectators } =
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
                [ Events.onSubmit [Msg.InRoom (Msg.SendMessage messageEntry)] ]
                [ Html.input
                    [ Attributes.type_ "text"
                    , Attributes.placeholder "chat"
                    , Attributes.value messageEntry
                    , Events.onInput (\s -> [Msg.InRoom (Msg.ComposeMessage s)])
                    , Events.onFocus [Msg.InRoom (Msg.SetUseKeysForGame False)]
                    , Events.onBlur [Msg.InRoom (Msg.SetUseKeysForGame True)]
                    ]
                    []
                ]
            ]
        ]

    historyRow item =
      let
        (username, content) =
          case item of
            Model.JoinedRoom joiner -> (joiner, [ Html.text "joined the room" ])
            Model.LeftRoom leaver -> (leaver, [ Html.text "left the room" ])
            Model.JoinedGame leaver -> (leaver, [ Html.text "joined the game" ])
            Model.Chatted { sender, message } ->
              ( sender
              , [ Html.text (": " ++ message) ]
              )
            Model.PlayerMoved { player, moveReport } ->
              case moveReport of
                Model.PlayedWord { words, score } ->
                  ( player
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
                Model.Exchanged numTiles ->
                  ( player
                  , [ Html.text "exchanged "
                    , Html.text (String.fromInt numTiles)
                    , Html.text " tiles"
                    ]
                  )
                Model.Passed -> ( player, [ Html.text "passed their turn" ] )
                Model.Undone -> ( player, [ Html.text "undid the last move" ] )
            Model.GameOver ->
              ( ""
              , [ Html.strong [] [ Html.text "The game has ended!" ] ]
              )
            Model.NewGameStarted { by } ->
              ( ""
              , [ Html.strong [] [ Html.text by, Html.text " started a new game" ]
                , Html.text " (if you didn't want this, "
                , Html.a
                    [ Attributes.href "#"
                    , Events.onClick [Msg.InRoom Msg.SendUndo]
                    ]
                    [ Html.text "undo it" ]
                , Html.text ")"
                ]
              )
      in
      Html.tr
        []
        [ Html.td [] [ Html.text username ]
        , Html.td [] content
        ]
  in
  Html.div
    []
    (List.concat
      [ if Set.isEmpty spectators
        then []
        else
          [ Html.text "Spectators: "
          , Html.text (String.join ", " (Set.toList spectators))
          ]
      , [ Html.table [] (inputRow :: List.map historyRow history) ]
      ]
    )

viewHelp : Html Msg
viewHelp =
  Html.div
    []
    [ Html.p
        []
        [ Html.strong [] [ Html.text "Basics: " ]
        , Html.text """
          Make moves by clicking on a starting square, once for right, twice for
          down, then type the letters of your word, including any already on the
          board. Press enter to submit. Blanks are placed with the space key.
          The game doesn't know what letter they are, you will have to agree
          that amongst yourselves in the chat. Press backspace to remove letters
          from your word, and you can press escape to cancel an empty word, or
          just click somewhere else to start a new one. Exchanging works very
          similarly.
          """
        ]
    , Html.p
        []
        [ Html.strong [] [ Html.text "Weirdnesses: " ]
        , Html.text """
          Any player can undo any move. There is no automatic dictionary
          checking, and there's no explicit support for challenges: you can
          emulate them with undo and pass. The game doesn't forbid players from
          joining during the game, but it can have some weird results. You can
          try undoing the move before they joined to kick them out again.
          """
        ]
    ]

view : Model.Model -> Browser.Document Msg
view { error, state, showAbout, muted } =
  let
    title =
      [ [ Html.text "skerrible | "
        , Html.a
            [ Attributes.href "#"
            , Events.onClick [Msg.Global (Msg.SetShowAbout (not showAbout))]
            ]
            [ Html.text
              <| if showAbout
                  then "return"
                  else "about"
            ]
        , Html.text " | "
        , Html.a
            [ Attributes.href "#"
            , Events.onClick [Msg.Global (Msg.SetMuted (not muted))]
            ]
            [ Html.text
              <| if muted
                  then "unmute"
                  else "mute"
            ]
        ]
      , case state of
          Model.PreLogin _ -> []
          Model.InRoom { game } ->
            let
              setLink setTo =
                Html.a
                  [ Attributes.href "#"
                  , Events.onClick [Msg.InRoom (Msg.SetHelpVisible setTo)]
                  ]
                  [ Html.text (
                      if setTo then "show help" else "hide help"
                    )
                  ]
            in
            if game.showHelp
            then
              [ viewHelp
              , Html.p [] [ setLink False ]
              ]
            else
              [ Html.text " | "
              , setLink True
              ]
      , [ Html.hr [] [] ]
      ] |> List.concat

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
                  , Events.onClick [Msg.Global (Msg.SetError Nothing)]
                  ]
                  [ Html.text "clear" ]
              ]
          ]

    aboutDisplay =
      [ Html.p
          []
          [ Html.text "skerrible is on "
          , Html.a
              [ Attributes.href "https://github.com/bmillwood/skerrible" ]
              [ Html.text "github" ]
          ]
      , Html.p
          []
          [ Html.text "the move sound is a "
          , Html.a
              [ Attributes.href "https://github.com/bmillwood/skerrible/blob/main/web-client/make-media.sh" ]
              [ Html.text "lightly modified" ]
          , Html.text " sound by "
          , Html.a
              [ Attributes.href "https://freesound.org/people/jakebagger/sounds/499782/" ]
              [ Html.text "Jake Bagger" ]
          , Html.text ", used under CC-BY 4.0"
          ]
      ]

    stateDisplay =
      [ case state of
          Model.PreLogin preLogin -> viewPreLogin preLogin
          Model.InRoom { chat, game, roomCode } ->
            let
              roomCodeDisplay =
                Html.p []
                  [ Html.text "Room code: "
                  , Html.text roomCode
                  , Html.text " "
                  , Html.a
                      [ Attributes.href ("?room=" ++ roomCode) ]
                      [ Html.text "link" ]
                  ]

              boardDisplay =
                viewBoard
                  { board = game.board
                  , tileData = game.tileData
                  , proposedMove =
                      case game.playing |> Maybe.andThen .proposal of
                        Nothing -> Nothing
                        Just (Move.ProposeMove move) -> Just move
                        Just (Move.ProposeExchange _) -> Nothing
                  , transientError = game.transientError
                  , gameOver = game.gameOver
                  }

              rackOrJoin =
                case game.playing of
                  Nothing ->
                    [ Html.button
                        [ Events.onClick [Msg.InRoom Msg.SendJoin] ]
                        [ Html.text "Join game" ]
                    ]
                  Just { proposal, rack } ->
                    [ viewRack
                        { rack =
                            case proposal of
                              Nothing -> rack
                              Just p -> Move.remainingRack p rack
                        , tileData = game.tileData
                        , proposedExchange =
                            case proposal of
                              Nothing -> Nothing
                              Just (Move.ProposeMove _) -> Nothing
                              Just (Move.ProposeExchange tiles) -> Just tiles
                        , gameOver = game.gameOver
                        , rackError = game.transientError == Just Model.RackError
                        }
                    ]

              spectators =
                Set.diff
                  chat.folks
                  (Set.fromList (Dict.keys game.players))
            in
            Html.div
              []
              (List.concat
                [ [ roomCodeDisplay
                  , boardDisplay
                  , viewPlayers game.players
                  , Html.div
                      []
                      [ Html.button
                          [ Events.onClick [Msg.InRoom Msg.SendUndo] ]
                          [ Html.text "Undo" ]
                      , Html.button
                          [ Events.onClick [Msg.InRoom Msg.SendStartNewGame] ]
                          [ Html.text "Start new game" ]
                      ]
                  , Html.hr [ Attributes.style "clear" "both" ] []
                  , viewError game.moveError
                  ]
                , rackOrJoin
                , [ Html.hr [ Attributes.style "clear" "both" ] []
                  , viewChatting chat { spectators = spectators }
                  ]
                ]
              )
      ]
  in
  { title = "Skerrible"
  , body =
      [ title
      , errorDisplay
      , if showAbout then aboutDisplay else stateDisplay
      ] |> List.concat
  }
