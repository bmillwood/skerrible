module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict
import Json.Decode
import Html exposing (Html)
import Random
import Random.List
import Set
import Task

import Board exposing (Board)
import DictTile
import FlagsParser
import Key exposing (Key)
import Model exposing (Model)
import Move exposing (Move)
import Msg exposing (Msg)
import Ports
import View

init : Json.Decode.Value -> (Model, Cmd Msg)
init rawFlags =
  let
    { error, endpoint, username, room, autoLogin, turns }
      = FlagsParser.parseFlags rawFlags
  in
  ( { error = error
    , state =
        Model.PreLogin
          { loginState = Model.NotSubmitted
          , loginForm =
              { endpoint = endpoint
              , username = username
              , roomAction =
                  case room of
                    Nothing -> Model.MakeNewRoom
                    Just _ -> Model.JoinRoom
              , roomCode = Maybe.withDefault "" room
              , roomSettings =
                  { noBoardMultipliers = False
                  , turnEnforcement = turns
                  }
              }
          }
    }
  , if autoLogin
    then Task.perform identity (Task.succeed (Ok (Msg.PreLogin Msg.Submit)))
    else Cmd.none
  )

updates : List Msg -> Model -> (Model, Cmd Msg)
updates msgs model =
  case msgs of
    [] -> (model, Cmd.none)
    firstMsg :: restMsgs ->
      let
        (nextModel, nextCmd) = update firstMsg model
        (lastModel, lastCmd) = updates restMsgs nextModel
      in
      (lastModel, Cmd.batch [nextCmd, lastCmd])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.state of
    Model.PreLogin preLogin ->
      let
        set newPreLogin = { model | state = Model.PreLogin newPreLogin }
        failed error =
          ( { model
            | state = Model.PreLogin { preLogin | loginState = Model.Failed }
            , error = Just error
            }
          , Cmd.none
          )
        loggedIn roomCode =
          ( { model | state =
                Model.InGame
                  { chat =
                      { folks = Set.empty
                      , me = preLogin.loginForm.username
                      , messageEntry = ""
                      , history = []
                      }
                  , game =
                      { board = Board.empty
                      , tileData = DictTile.empty
                      , scores = Dict.empty
                      , playing = Nothing
                      , moveError = Nothing
                      , transientError = Nothing
                      , showHelp = True
                      }
                  , roomCode = roomCode
                  }
            }
          , Cmd.none
          )
      in
      case msg of
        Err error -> failed (Msg.errorToString error)
        Ok Msg.ClearError -> ({ model | error = Nothing }, Cmd.none)
        Ok Msg.ClearMoveError -> (model, Cmd.none)
        Ok (Msg.Many msgs) -> updates (List.map Ok msgs) model
        Ok (Msg.UpdateRoomCode code) -> loggedIn code
        Ok (Msg.PreLogin loginMsg) ->
          case loginMsg of
            Msg.Update newForm ->
              ( set { preLogin | loginForm = newForm }, Cmd.none )
            Msg.Submit ->
              ( set { preLogin | loginState = Model.Waiting }
              , Ports.connect { endpoint = preLogin.loginForm.endpoint }
              )
            Msg.Connected ->
              ( model
              , Ports.login preLogin.loginForm
              )
            Msg.NoSuchRoom ->
              failed "Room does not exist"
        Ok other ->
          failed ("Ingame-only message outside of game: " ++ Debug.toString other)
    Model.InGame ({ chat, game } as inGame) ->
      let
        setChat newChat = { model | state = Model.InGame { inGame | chat = newChat } }
        setGame newGame = { model | state = Model.InGame { inGame | game = newGame } }
        setPlaying newPlaying = setGame { game | playing = Just newPlaying }
        error errorMsg = ( { model | error = Just errorMsg }, Cmd.none )
      in
      case msg of
        Err errorMsg -> error (Msg.errorToString errorMsg)
        Ok (Msg.PreLogin _) -> error "Pre-login message after login"
        Ok Msg.ClearError -> ( { model | error = Nothing }, Cmd.none )
        Ok (Msg.Many msgs) -> updates (List.map Ok msgs) model
        Ok (Msg.UpdateRoomCode code) ->
          ( { model | state = Model.InGame { inGame | roomCode = code } }, Cmd.none )
        Ok (Msg.ComposeMessage composed) ->
          ( setChat { chat | messageEntry = composed }, Cmd.none )
        Ok (Msg.SendMessage message) ->
          ( setChat { chat | messageEntry = "" }
          , Ports.chat message
          )
        Ok (Msg.ReceiveChatMessage chatMsg) ->
          ( setChat { chat | history = Model.Chatted chatMsg :: chat.history }
          , Cmd.none
          )
        Ok Msg.SendJoin -> ( model, Ports.joinGame )
        Ok (Msg.ReceiveMove moveReport) ->
          ( setChat { chat | history = Model.PlayerMoved moveReport :: chat.history }
          , Cmd.none
          )
        Ok Msg.GameOver ->
          ( setChat { chat | history = Model.GameOver :: chat.history }
          , Cmd.none
          )
        Ok (Msg.UpdateBoard newBoard) ->
          ( setGame { game | board = newBoard }, Cmd.none )
        Ok (Msg.UpdateTileData tileData) ->
          ( setGame { game | tileData = tileData }, Cmd.none )
        Ok (Msg.UpdateRack newRack) ->
          ( setPlaying { rack = newRack, proposal = Nothing }, Cmd.none )
        Ok (Msg.ShuffleRack Nothing) ->
          ( model
          , case game.playing of
              Nothing -> Cmd.none
              Just { rack } ->
                -- Generate a list of indices instead of a shuffled rack to avoid
                -- overwriting in-flight racks from the server.
                Random.generate
                  (Ok << Msg.ShuffleRack << Just)
                  (Random.List.shuffle (List.indexedMap (\i _ -> i) rack))
          )
        Ok (Msg.ShuffleRack (Just indices)) ->
          case game.playing of
            Nothing -> ( model, Cmd.none )
            Just ({ rack } as playing) ->
              let
                allIndicesPresent =
                  List.all
                    (\i -> List.member i indices)
                    (List.indexedMap (\i _ -> i) rack)
                newRack = List.filterMap (\i -> List.head (List.drop i rack)) indices
              in
              if allIndicesPresent
              then ( setPlaying { playing | rack = newRack }, Cmd.none )
              else
                -- Ignoring is fine. You can just click the button again.
                ( model, Cmd.none )
        Ok (Msg.SetTransientError newTransientError) ->
          ( setGame { game | transientError = newTransientError }, Cmd.none )
        Ok (Msg.UpdateProposal proposalUpdate) ->
          case game.playing of
            Nothing -> ( model, Cmd.none )
            Just { proposal, rack } ->
              case proposal of
                Nothing -> ( model, Cmd.none )
                Just prop ->
                  update
                    (Ok (updateProposal game.board rack prop proposalUpdate))
                    model
        Ok (Msg.Propose proposal) ->
          case game.playing of
            Nothing -> ( model, Cmd.none )
            Just playing ->
              ( setPlaying { playing | proposal = proposal }, Cmd.none )
        Ok Msg.SendProposal ->
          case game.playing |> Maybe.andThen .proposal of
            Nothing -> ( model, Cmd.none )
            Just (Move.ProposeMove move) -> ( model, Ports.sendMove move )
            Just (Move.ProposeExchange tiles) -> ( model, Ports.sendExchange tiles )
        Ok Msg.SendPass -> ( model, Ports.sendPass )
        Ok Msg.SendUndo -> ( model, Ports.sendUndo )
        Ok (Msg.MoveResult (Err moveError)) ->
          ( setGame { game | moveError = Just moveError }, Cmd.none )
        Ok (Msg.MoveResult (Ok ())) ->
          let
            newPlaying =
              case game.playing of
                Nothing -> Nothing
                Just { rack } -> Just { rack = rack, proposal = Nothing }
          in
          ( setGame { game | moveError = Nothing, playing = newPlaying }, Cmd.none )
        Ok Msg.ClearMoveError ->
          ( setGame { game | moveError = Nothing }, Cmd.none )
        Ok (Msg.UpdatePeople newFolks) ->
          let
            added =
              Set.diff newFolks chat.folks
              |> Set.toList |> List.map Model.JoinedRoom
            removed =
              Set.diff chat.folks newFolks
              |> Set.toList |> List.map Model.LeftRoom
          in
          ( { model
            | state = Model.InGame
                { inGame
                | chat =
                  { chat
                  | folks = newFolks
                  , history = added ++ removed ++ chat.history
                  }
                }
            }
          , Cmd.none
          )
        Ok (Msg.UpdateScores newScores) ->
          let
            newPlayers =
              Set.diff
                (Set.fromList (Dict.keys newScores))
                (Set.fromList (Dict.keys game.scores))
              |> Set.toList |> List.map Model.JoinedGame
          in
          ( { model
            | state = Model.InGame
                { inGame
                | chat = { chat | history = newPlayers ++ chat.history }
                , game = { game | scores = newScores }
                }
            }
          , Cmd.none
          )
        Ok (Msg.BlurById blurId) ->
          ( model
          , Task.attempt (\_ -> Ok Msg.doNothing) (Browser.Dom.blur blurId)
          )
        Ok (Msg.SetHelpVisible showHelp) ->
          ( setGame { game | showHelp = showHelp }, Cmd.none )

updateProposal
  : Board -> Board.Rack -> Move.Proposal -> Msg.ProposalUpdate -> Msg.OkMsg
updateProposal board rack proposal proposalUpdate =
  let
    deleteLast tiles = List.take (List.length tiles - 1) tiles
    updateMove move = Msg.Propose (Just (Move.ProposeMove move))
    updateExchange newTiles = Msg.Propose (Just (Move.ProposeExchange newTiles))
    ifHave tile addTile =
      if List.member tile (Move.remainingRack proposal rack)
      then addTile
      else Msg.SetTransientError (Just Model.RackError)
    cancelProposalIfEmpty tiles =
      if List.isEmpty tiles
      then Msg.Propose Nothing
      else Msg.doNothing
  in
  case (proposal, proposalUpdate) of
    (Move.ProposeMove move, Msg.UnproposeLast) ->
      updateMove { move | tiles = deleteLast move.tiles }
    (Move.ProposeExchange tiles, Msg.UnproposeLast) ->
      updateExchange (deleteLast tiles)
    (_, Msg.SubmitProposal) -> Msg.SendProposal
    (Move.ProposeExchange tiles, Msg.ProposeTile tile) ->
      ifHave tile (updateExchange (tiles ++ [tile]))
    (Move.ProposeMove move, Msg.ProposeTile tile) ->
      let
        (i, j) = Move.nextPos move
        addTile moveTile = updateMove { move | tiles = move.tiles ++ [moveTile] }
      in
      case Board.get i j board of
        Nothing -> Msg.SetTransientError (Just Model.BoardError)
        Just sq ->
          case sq.tile of
            Nothing ->
              ifHave tile (addTile (Move.PlaceTile tile))
            Just squareTile ->
              if tile == squareTile
              then addTile Move.UseBoard
              else Msg.SetTransientError (Just (Model.SquareError i j))
    (Move.ProposeMove move, Msg.CancelProposal) ->
      cancelProposalIfEmpty move.tiles
    (Move.ProposeExchange tiles, Msg.CancelProposal) ->
      cancelProposalIfEmpty tiles

handleKey : Json.Decode.Decoder Msg.OkMsg
handleKey =
  let
    ofKey key =
      case key of
        Key.Escape -> Msg.UpdateProposal Msg.CancelProposal
        Key.Enter -> Msg.UpdateProposal Msg.SubmitProposal
        Key.Backspace -> Msg.UpdateProposal Msg.UnproposeLast
        Key.Char c ->
          case Board.tileOfChar c of
            Nothing -> Msg.doNothing
            Just tile -> Msg.UpdateProposal (Msg.ProposeTile tile)
        Key.Other -> Msg.doNothing
  in
  Json.Decode.map ofKey Key.decodeKey

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    ifPlaying decoder =
      case model.state of
        Model.PreLogin _ -> Json.Decode.succeed (Ok Msg.doNothing)
        Model.InGame _ -> Json.Decode.map Ok decoder
    clearError = ifPlaying (Json.Decode.succeed (Msg.SetTransientError Nothing))
  in
  Sub.batch
    [ Ports.subscriptions model
    , Browser.Events.onKeyDown (ifPlaying handleKey)
    , Browser.Events.onKeyUp clearError
    , Browser.Events.onMouseUp clearError
    , Browser.Events.onVisibilityChange (\_ ->
          case model.state of
            Model.PreLogin _ -> Ok Msg.doNothing
            Model.InGame _ -> Ok (Msg.SetTransientError Nothing)
        )
    ]

main =
  Browser.document
    { init          = init
    , view          = View.view
    , update        = update
    , subscriptions = subscriptions
    }
