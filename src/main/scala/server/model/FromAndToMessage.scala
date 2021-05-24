package server.model

import game.model.{Room, User}

sealed trait InputMessage { val user: User }

case class Help(user: User) extends InputMessage

case class Chat(user: User, text: String) extends InputMessage

case class EnterRoom(user: User, room: Room) extends InputMessage

case class ListRooms(user: User) extends InputMessage

case class ListMembers(user: User) extends InputMessage

case class Disconnect(user: User) extends InputMessage

case class InvalidInput(user: User, text: String) extends InputMessage

case class StartGameInRoom(user: User) extends InputMessage

case class Round(user: User, combinations: String, dice: String) extends InputMessage

sealed trait OutputMessage {

  def forUser(targetUser: User): Boolean

  def toString: String
}

case class WelcomeUser(user: User) extends OutputMessage {
  override def forUser(targetUser: User): Boolean = targetUser == user
  override def toString: String                   = s"Welcome to Yahtzee"
}

case class SendToUser(user: User, text: String) extends OutputMessage {
  override def forUser(targetUser: User): Boolean = targetUser == user
  override def toString: String                   = text
}

case class SendToUsers(users: Set[User], text: String) extends OutputMessage {
  override def forUser(targetUser: User): Boolean = users.contains(targetUser)
  override def toString: String                   = text
}

object InputMessage {
  val DefaultRoomName = Room("default")
  val HelpText: String =
    """Commands:
        |  /help                       - Show this text
        |  /room                       - Change to default/entry room
        |  /room <room name>           - Change to specified room
        |  /rooms                      - List all rooms
        |  /members                    - List members in current room
        |  /start                      - Start game
        |  /round <Combination> <Dice> - Round game
    """.stripMargin

  // Parses a string into a command
  def parse(user: User, text: String): InputMessage =
    splitFirstTwoWords(text) match {
      case ("/help", _, _)     => Help(user)
      case ("/room", "", "")   => EnterRoom(user, DefaultRoomName)
      case ("/room", room, "") => EnterRoom(user, Room(room.toLowerCase))
      case ("/room", _, _) =>
        InvalidInput(user, "/room takes a single, optional argument")
      case ("/rooms", _, _)              => ListRooms(user)
      case ("/members", _, _)            => ListMembers(user)
      case ("/start", _, _)              => StartGameInRoom(user)
      case ("/round", combination, dice) => Round(user, combination, dice)
      case (s"/$cmd", _, _)              => InvalidInput(user, s"unknown command - $cmd")
      case _                             => Chat(user, text)
    }

  private def splitFirstTwoWords(text: String): (String, String, String) = {
    val (first, intermediate) = splitFirstWord(text)
    val (second, rest)        = splitFirstWord(intermediate)

    (first, second, rest)
  }

  private def splitFirstWord(text: String): (String, String) = {
    val trimmedText = text.trim
    val firstSpace  = trimmedText.indexOf(' ')
    if (firstSpace < 0)
      (trimmedText, "")
    else
      (
        trimmedText.substring(0, firstSpace),
        trimmedText.substring(firstSpace + 1).trim
      )
  }
}

case object KeepAlive extends OutputMessage {
  override def forUser(targetUser: User) = true
  override def toString: String          = ""
}

sealed trait Message { val text: String }
sealed trait ErrorMessage     extends Message
sealed trait RoomErrorMessage extends Message
sealed trait GameErrorMessage extends Message

object Message {

  object ErrorMessage {
    object Input extends ErrorMessage {
      val text = "Invalid input: "
    }

    object Initialization extends ErrorMessage {
      val text = "Player initialization error"
    }
  }

  object RoomErrorMessage {
    object NotInRoom extends RoomErrorMessage {
      val text = "You are not currently in a room"
    }

    object InRoom extends RoomErrorMessage {
      val text = "You are already in that room!"
    }

    object RoomOccupied extends RoomErrorMessage {
      val text = "The room is occupied"
    }

    object ChooseRoom extends RoomErrorMessage {
      val text = "Choose or create another room"
    }

    object SizeRoom extends RoomErrorMessage {
      val text = "The room is designed for a larger number of people"
    }
  }

  object GameErrorMessage {

    object NotYourMove extends GameErrorMessage {
      val text = "Now it's the other player's turn"
    }

    object NotFinished extends GameErrorMessage {
      val text = "Not everyone finished the game"
    }

    object ImpossibleCombination extends GameErrorMessage {
      val text = "This combination is impossible! Choose a combination, please"
    }

    object RollDice extends GameErrorMessage {
      val text = "You have spent all the rolls, choose a combination"
    }

    object StartGame extends GameErrorMessage {
      val text = "The game began"
    }
  }
}
