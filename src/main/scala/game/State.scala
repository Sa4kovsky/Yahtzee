package game

import game.Room.{addToRoom, removeFromCurrentRoom, sendToRoom, sizeRoom}
import game.YahtzeeGame._
import game.model.Player._
import server.model._

case class State(userRooms: Map[String, String], roomMembers: Map[String, Set[String]], player: Map[String, Player]) {
  val DefaultCountPlayerInRoom = 2
  val DefaultStep              = 3

  def process(msg: InputMessage): (State, Seq[OutputMessage]) = msg match {
    case Help(user) =>
      (this, Seq(SendToUser(user, InputMessage.HelpText)))

    case Chat(user, text) =>
      userRooms.get(user) match {
        case Some(room) =>
          (this, sendToRoom(room, s"$user: $text")(this.roomMembers))

        case None =>
          (this, Seq(SendToUser(user, Message.RoomErrorMessage.NotInRoom.text)))
      }

    case EnterRoom(user, toRoom) =>
      userRooms.get(user) match {
        case None =>
          val (finalState, enterMessages) = addToRoom(user, toRoom)(this)
          (finalState, Seq(WelcomeUser(user)) ++ enterMessages)

        case Some(currentRoom) if currentRoom == toRoom =>
          (this, Seq(SendToUser(user, Message.RoomErrorMessage.InRoom.text)))

        case Some(_) =>
          if (sizeRoom(toRoom)(roomMembers) >= DefaultCountPlayerInRoom)
            (this, Seq(SendToUser(user, Message.RoomErrorMessage.RoomOccupied.text)))
          else {
            val (intermediateState, leaveMessages) = removeFromCurrentRoom(user)(this)
            val (finalState, enterMessages)        = addToRoom(user, toRoom)(intermediateState)
            (finalState, leaveMessages ++ enterMessages)
          }
      }

    case ListRooms(user) =>
      val roomList = roomMembers.keys.toList.sorted.mkString("Rooms:\n\t", "\n\t", "")
      (this, Seq(SendToUser(user, roomList)))

    case ListMembers(user) =>
      val memberList = userRooms.get(user) match {
        case Some(room) =>
          roomMembers
            .getOrElse(room, Set())
            .toList
            .sorted
            .mkString("Room Members:\n\t", "\n\t", "")

        case None =>
          Message.RoomErrorMessage.NotInRoom.text
      }
      (this, Seq(SendToUser(user, memberList)))

    case StartGameInRoom(user) =>
      userRooms.get(user) match {
        case Some(room) =>
          if (room != "default") {
            startGame(user, room, DefaultCountPlayerInRoom)(this)
          } else
            (this, sendToRoom(room, Message.RoomErrorMessage.ChooseRoom.text)(this.roomMembers))

        case None => (this, Seq(SendToUser(user, Message.RoomErrorMessage.NotInRoom.text)))
      }

    case Round(user, combinations, dice) =>
      userRooms.get(user) match {
        case Some(room) =>
          val users: List[String] = roomMembers.getOrElse(room, List()).toList
          if (user == users.head) {
            player.get(user) match {
              case Some(bettor) =>
                game(dice: String, bettor: Player, room: String, user: String, combinations: String)(this)
              case None =>
                (this, Seq(SendToUser(user, Message.ErrorMessage.Initialization.text)))
            }
          } else {
            (this, Seq(SendToUser(user, Message.GameErrorMessage.NotYourMove.text)))
          }
        case None =>
          (this, Seq(SendToUser(user, Message.RoomErrorMessage.NotInRoom.text)))
      }

    case Disconnect(user) =>
      removeFromCurrentRoom(user)(this)

    case InvalidInput(user, text) =>
      (this, Seq(SendToUser(user, Message.ErrorMessage.Input.text + text)))
  }

}

object State {
  def apply(): State = State(Map.empty, Map.empty, Map.empty)
}
