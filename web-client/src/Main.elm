module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Dict
import Json.Decode
import Html exposing (Html)
import Random
import Random.List
import Set
import Task
import Url exposing (Url)

import Board exposing (Board)
import DictTile
import FlagsParser
import Key exposing (Key)
import Model exposing (Model)
import Move exposing (Move)
import Msg exposing (Msg)
import Ports
import View

init : Json.Decode.Value -> Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init rawFlags url navKey =
  let
    { error, endpoint, username, room, autoLogin, turns }
      = FlagsParser.parseFlags rawFlags
  in
  ( { error = error
    , navKey = navKey
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
    then Task.perform identity (Task.succeed [Msg.PreLogin Msg.Submit])
    else Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msgs model =
  case msgs of
    [] -> (model, Cmd.none)
    firstMsg :: restMsgs ->
      let
        (nextModel, nextCmd) = updateOne firstMsg model
        (lastModel, lastCmd) = update restMsgs nextModel
      in
      (lastModel, Cmd.batch [nextCmd, lastCmd])

updatePreLogin
  : Msg.LoginFormMsg -> Model -> Model.PreLoginState -> (Model, Cmd Msg)
updatePreLogin msg model state =
  let
    set newPreLogin =
      { model | state = Model.PreLogin newPreLogin }
    failed newError =
      ( { model
        | state = Model.PreLogin { state | loginState = Model.Failed }
        , error = Just newError
        }
      , Cmd.none
      )
  in
  case msg of
    Msg.UpdateRoomCode code ->
      ( { model
        | state =
            Model.InGame
              { chat =
                  { folks = Set.empty
                  , me = state.loginForm.username
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
              , roomCode = code
              }
        , error = Nothing
        }
      , Cmd.none
      )
    Msg.Update newForm ->
      ( set { state | loginForm = newForm }, Cmd.none )
    Msg.Submit ->
      ( set { state | loginState = Model.Waiting }
      , Ports.connect { endpoint = state.loginForm.endpoint }
      )
    Msg.Connected ->
      ( set state
      , Ports.login state.loginForm
      )
    Msg.NoSuchRoom ->
      failed "Room does not exist"

updateOne : Msg.OneMsg -> Model -> (Model, Cmd Msg)
updateOne msg model =
  case (model.state, msg) of
    (_, Msg.Global (Msg.SetError e)) ->
      ( { model | error = e }, Cmd.none )
    (_, Msg.Global (Msg.BlurById blurId)) ->
      ( model
      , Task.attempt (\_ -> Msg.doNothing) (Browser.Dom.blur blurId)
      )
    (_, Msg.Global (Msg.UrlRequest (Browser.Internal _))) ->
      ( model, Cmd.none )
    (_, Msg.Global (Msg.UrlChange url)) ->
      ( model, Cmd.none )
    (_, Msg.Global (Msg.UrlRequest (Browser.External url))) ->
      ( model, Browser.Navigation.load url )
    (Model.PreLogin preLogin, Msg.PreLogin loginFormMsg) ->
      updatePreLogin loginFormMsg model preLogin
    (Model.PreLogin _, _) ->
      ( { model | error = Just "Bug: inappropriate message type for pre-login state" }
      , Cmd.none
      )
    (Model.InGame ({ chat, game } as inGame), Msg.PreLogin _) ->
      ( { model | error = Just "Bug: inappropriate message type for in-game state" }
      , Cmd.none
      )
    (Model.InGame ({ chat, game } as inGame), Msg.InGame gameMsg) ->
      let
        setChat newChat = { model | state = Model.InGame { inGame | chat = newChat } }
        setGame newGame = { model | state = Model.InGame { inGame | game = newGame } }
        setPlaying newPlaying = setGame { game | playing = Just newPlaying }
      in
      case gameMsg of
        Msg.ComposeMessage composed ->
          ( setChat { chat | messageEntry = composed }, Cmd.none )
        Msg.SendMessage message ->
          ( setChat { chat | messageEntry = "" }
          , Ports.chat message
          )
        Msg.ReceiveChatMessage chatMsg ->
          ( setChat { chat | history = Model.Chatted chatMsg :: chat.history }
          , Cmd.none
          )
        Msg.SendJoin -> ( model, Ports.joinGame )
        Msg.ReceiveMove moveReport ->
          ( setChat { chat | history = Model.PlayerMoved moveReport :: chat.history }
          , Cmd.none
          )
        Msg.GameOver ->
          ( setChat { chat | history = Model.GameOver :: chat.history }
          , Cmd.none
          )
        Msg.UpdateBoard newBoard ->
          ( setGame { game | board = newBoard }, Cmd.none )
        Msg.UpdateTileData tileData ->
          ( setGame { game | tileData = tileData }, Cmd.none )
        Msg.UpdateRack newRack ->
          ( setPlaying { rack = newRack, proposal = Nothing }, Cmd.none )
        Msg.ShuffleRack Nothing ->
          ( model
          , case game.playing of
              Nothing -> Cmd.none
              Just { rack } ->
                -- Generate a list of indices instead of a shuffled rack to avoid
                -- overwriting in-flight racks from the server.
                Random.generate
                  (\indices -> [Msg.InGame (Msg.ShuffleRack (Just indices))])
                  (Random.List.shuffle (List.indexedMap (\i _ -> i) rack))
          )
        Msg.ShuffleRack (Just indices) ->
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
        Msg.SetTransientError newTransientError ->
          ( setGame { game | transientError = newTransientError }, Cmd.none )
        Msg.UpdateProposal proposalUpdate ->
          case game.playing of
            Nothing -> ( model, Cmd.none )
            Just { proposal, rack } ->
              case proposal of
                Nothing -> ( model, Cmd.none )
                Just prop ->
                  update
                    (updateProposal game.board rack prop proposalUpdate)
                    model
        Msg.Propose proposal ->
          case game.playing of
            Nothing -> ( model, Cmd.none )
            Just playing ->
              ( setPlaying { playing | proposal = proposal }, Cmd.none )
        Msg.SendProposal ->
          case game.playing |> Maybe.andThen .proposal of
            Nothing -> ( model, Cmd.none )
            Just (Move.ProposeMove move) -> ( model, Ports.sendMove move )
            Just (Move.ProposeExchange tiles) -> ( model, Ports.sendExchange tiles )
        Msg.SendPass -> ( model, Ports.sendPass )
        Msg.SendUndo -> ( model, Ports.sendUndo )
        Msg.MoveResult (Err moveError) ->
          ( setGame { game | moveError = Just moveError }, Cmd.none )
        Msg.MoveResult (Ok ()) ->
          let
            newPlaying =
              case game.playing of
                Nothing -> Nothing
                Just { rack } -> Just { rack = rack, proposal = Nothing }
          in
          ( setGame { game | moveError = Nothing, playing = newPlaying }, Cmd.none )
        Msg.ClearMoveError ->
          ( setGame { game | moveError = Nothing }, Cmd.none )
        Msg.UpdatePeople newFolks ->
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
        Msg.UpdateScores newScores ->
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
        Msg.SetHelpVisible showHelp ->
          ( setGame { game | showHelp = showHelp }, Cmd.none )

updateProposal
  : Board -> Board.Rack -> Move.Proposal -> Msg.ProposalUpdate -> Msg
updateProposal board rack proposal proposalUpdate =
  let
    deleteLast tiles = List.take (List.length tiles - 1) tiles
    propose p = [Msg.InGame (Msg.Propose p)]
    updateMove move = propose (Just (Move.ProposeMove move))
    updateExchange newTiles = propose (Just (Move.ProposeExchange newTiles))
    ifHave tile addTile =
      if List.member tile (Move.remainingRack proposal rack)
      then addTile
      else [Msg.InGame (Msg.SetTransientError (Just Model.RackError))]
    cancelProposalIfEmpty tiles =
      if List.isEmpty tiles
      then propose Nothing
      else Msg.doNothing
  in
  case (proposal, proposalUpdate) of
    (Move.ProposeMove move, Msg.UnproposeLast) ->
      updateMove { move | tiles = deleteLast move.tiles }
    (Move.ProposeExchange tiles, Msg.UnproposeLast) ->
      updateExchange (deleteLast tiles)
    (_, Msg.SubmitProposal) -> [Msg.InGame Msg.SendProposal]
    (Move.ProposeExchange tiles, Msg.ProposeTile tile) ->
      ifHave tile (updateExchange (tiles ++ [tile]))
    (Move.ProposeMove move, Msg.ProposeTile tile) ->
      let
        (i, j) = Move.nextPos move
        addTile moveTile = updateMove { move | tiles = move.tiles ++ [moveTile] }
      in
      case Board.get i j board of
        Nothing -> [Msg.InGame (Msg.SetTransientError (Just Model.BoardError))]
        Just sq ->
          case sq.tile of
            Nothing ->
              ifHave tile (addTile (Move.PlaceTile tile))
            Just squareTile ->
              if tile == squareTile
              then addTile Move.UseBoard
              else [Msg.InGame (Msg.SetTransientError (Just (Model.SquareError i j)))]
    (Move.ProposeMove move, Msg.CancelProposal) ->
      cancelProposalIfEmpty move.tiles
    (Move.ProposeExchange tiles, Msg.CancelProposal) ->
      cancelProposalIfEmpty tiles

handleKey : Json.Decode.Decoder Msg
handleKey =
  let
    updateP u = [Msg.InGame (Msg.UpdateProposal u)]
    ofKey key =
      case key of
        Key.Escape -> updateP Msg.CancelProposal
        Key.Enter -> updateP Msg.SubmitProposal
        Key.Backspace -> updateP Msg.UnproposeLast
        Key.Char c ->
          case Board.tileOfChar c of
            Nothing -> Msg.doNothing
            Just tile -> updateP (Msg.ProposeTile tile)
        Key.Other -> Msg.doNothing
  in
  Json.Decode.map ofKey Key.decodeKey

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    ifPlaying decoder =
      case model.state of
        Model.PreLogin _ -> Json.Decode.succeed Msg.doNothing
        Model.InGame _ -> decoder
    clearError = ifPlaying (Json.Decode.succeed [Msg.InGame (Msg.SetTransientError Nothing)])
  in
  Sub.batch
    [ Ports.subscriptions model
    , Browser.Events.onKeyDown (ifPlaying handleKey)
    , Browser.Events.onKeyUp clearError
    , Browser.Events.onMouseUp clearError
    , Browser.Events.onVisibilityChange (\_ ->
          case model.state of
            Model.PreLogin _ -> Msg.doNothing
            Model.InGame _ -> [Msg.InGame (Msg.SetTransientError Nothing)]
        )
    ]

main =
  Browser.application
    { init          = init
    , view          = View.view
    , update        = update
    , subscriptions = subscriptions
    , onUrlRequest  = \req -> [Msg.Global (Msg.UrlRequest req)]
    , onUrlChange   = \chg -> [Msg.Global (Msg.UrlChange chg)]
    }
