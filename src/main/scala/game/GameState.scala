package game

import _root_.game.model.Player.Player
import _root_.game.model.{Room, User}
import cats.Monad
import cats.data.State
import game.Room.{addToRoom, removeFromCurrentRoom, sizeRoom}
import game.Game._
import server.model._

case class GameState(userRooms: Map[User, Room], roomMembers: Map[Room, Set[User]], player: Map[User, Player]) {}

object GameState {
  val DefaultCountPlayerInRoom = 2
  val DefaultStep              = 3

  def apply(): GameState = GameState(Map.empty, Map.empty, Map.empty)

  def process(msg: InputMessage): State[GameState, Seq[OutputMessage]] =
    msg match {
      case Help(user) =>
        State.pure(Seq(SendToUser(user, InputMessage.HelpText)))

      case Chat(user, text) =>
        State { state =>
          state.userRooms.get(user) match {
            case Some(room) =>
              (state, Seq(SendToUsers(state.roomMembers.getOrElse(room, Set.empty), s"$user: $text")))

            case None =>
              (state, Seq(SendToUser(user, Message.RoomErrorMessage.NotInRoom.text)))
          }
        }

      case EnterRoom(user, toRoom) =>
        State { state =>
          state.userRooms.get(user) match {
            case None =>
              val (finalState, enterMessages) = addToRoom(user, toRoom)(state)
              (finalState, Seq(WelcomeUser(user)) ++ enterMessages)

            case Some(currentRoom) if currentRoom == toRoom =>
              (state, Seq(SendToUser(user, Message.RoomErrorMessage.InRoom.text)))

            case Some(_) =>
              if (sizeRoom(toRoom)(state) >= DefaultCountPlayerInRoom)
                (state, Seq(SendToUser(user, Message.RoomErrorMessage.RoomOccupied.text)))
              else {
                val (intermediateState, leaveMessages) = removeFromCurrentRoom(user)(state)
                val (finalState, enterMessages)        = addToRoom(user, toRoom)(intermediateState)
                (finalState, leaveMessages ++ enterMessages)
              }
          }
        }

      case ListRooms(user) =>
        State { state =>
          val roomList = state.roomMembers.keys.toList.sortBy(_.name).mkString("Rooms:\n\t", "\n\t", "")
          (state, Seq(SendToUser(user, roomList)))
        }

      case ListMembers(user) =>
        State { state =>
          val memberList = state.userRooms.get(user) match {
            case Some(room) =>
              state.roomMembers
                .getOrElse(room, Set())
                .toList
                .sortBy(_.name)
                .mkString("Room Members:\n\t", "\n\t", "")

            case None =>
              Message.RoomErrorMessage.NotInRoom.text
          }
          (state, Seq(SendToUser(user, memberList)))
        }

      case StartGameInRoom(user) =>
        State { state =>
          state.userRooms.get(user) match {
            case Some(room) =>
              if (room != Room("default")) {
                startGame(user, room, DefaultCountPlayerInRoom)(state)
              } else
                (
                  state,
                  Seq(
                    SendToUsers(state.roomMembers.getOrElse(room, Set.empty), Message.RoomErrorMessage.ChooseRoom.text)
                  )
                )

            case None => (state, Seq(SendToUser(user, Message.RoomErrorMessage.NotInRoom.text)))
          }
        }

      case Round(user, combinations, dice) =>
        State { state =>
          state.userRooms.get(user) match {
            case Some(room) =>
              val users: List[User] = state.roomMembers.getOrElse(room, List()).toList
              if (user == users.head) {
                state.player.get(user) match {
                  case Some(bettor) =>
                    game(dice: String, bettor: Player, room: Room, user: User, combinations: String)(state)
                  case None =>
                    (state, Seq(SendToUser(user, Message.ErrorMessage.Initialization.text)))
                }
              } else {
                (state, Seq(SendToUser(user, Message.GameErrorMessage.NotYourMove.text)))
              }
            case None =>
              (state, Seq(SendToUser(user, Message.RoomErrorMessage.NotInRoom.text)))
          }
        }

      case Disconnect(user) =>
        State { state =>
          removeFromCurrentRoom(user)(state)
        }

      case InvalidInput(user, text) =>
        State { state =>
          (state, Seq(SendToUser(user, Message.ErrorMessage.Input.text + text)))
        }
    }
}
