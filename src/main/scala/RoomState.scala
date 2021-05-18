import Player._
import YahtzeeGame.{diceRoll, resultGame, сalculationWeight}
import io.circe.jawn
import io.circe.syntax.EncoderOps

case class RoomState(
    userRooms: Map[String, String],
    roomMembers: Map[String, Set[String]],
    player: Map[String, Player]
) {
  val DefaultCountPlayerInRoom = 2
  val DefaultCombinationsDice = CombinationsDice.of()
  val DefaultPlayer = Player.of(List(DefaultCombinationsDice))
  val DefaultStep = 3

  def process(msg: InputMessage): (RoomState, Seq[OutputMessage]) = msg match {
    case Help(user) =>
      (this, Seq(SendToUser(user, InputMessage.HelpText)))

    case Chat(user, text) => {
      userRooms.get(user) match {
        case Some(room) =>
          (this, sendToRoom(room, s"$user: $text"))

        case None =>
          (this, Seq(SendToUser(user, "You are not currently in a room")))
      }
    }

    case EnterRoom(user, toRoom) => {
      userRooms.get(user) match {
        case None =>
          // Первый раз - добро пожаловать
          val (finalState, enterMessages) = addToRoom(user, toRoom)

          (finalState, Seq(WelcomeUser(user)) ++ enterMessages)

        case Some(currentRoom) if currentRoom == toRoom =>
          (this, Seq(SendToUser(user, "You are already in that room!")))

        case Some(_) =>
          if (sizeRoom(toRoom) >= DefaultCountPlayerInRoom)
            (this, Seq(SendToUser(user, "The room is occupied")))
          else {

            val (intermediateState, leaveMessages) = removeFromCurrentRoom(user)
            val (finalState, enterMessages) =
              intermediateState.addToRoom(user, toRoom)

            (finalState, leaveMessages ++ enterMessages)
          }
      }
    }

    case ListRooms(user) => {
      val roomList = roomMembers.keys.toList.sorted
        .mkString("Rooms:\n\t", "\n\t", "")

      (this, Seq(SendToUser(user, roomList)))
    }

    case ListMembers(user) => {
      val memberList = userRooms.get(user) match {
        case Some(room) =>
          roomMembers
            .getOrElse(room, Set())
            .toList
            .sorted
            .mkString("Room Members:\n\t", "\n\t", "")

        case None =>
          "You are not currently in a room"
      }
      (this, Seq(SendToUser(user, memberList)))
    }

    case StartGameInRoom(user) =>
      userRooms.get(user) match {
        case Some(room) =>
          if (room != "default") {
            (
              this,
              sendToRoom(
                room,
                Dice
                  .of(
                    diceRoll(0),
                    diceRoll(0),
                    diceRoll(0),
                    diceRoll(0),
                    diceRoll(0)
                  )
                  .asJson(Dice.encodeDice)
                  .toString()
              )
            )
          } else (this, sendToRoom(room, "Choose or create another room"))
        case None =>
          (this, Seq(SendToUser(user, "You are not currently in a room")))
      }

    case Round(user, dice, combinations) =>
      userRooms.get(user) match {
        case Some(room) =>
          if (player.head._1 == user) {
            player.get(user) match {
              case Some(bettor) =>
                jawn.decode(dice)(Dice.decodeDice) match {
                  case Right(value) =>
                    if (
                      value.a == 0 || value.b == 0 || value.c == 0 || value.d == 0 || value.e == 0 || combinations == Nothing.name
                    ) {
                      val dice = Dice.of(
                        diceRoll(value.a),
                        diceRoll(value.b),
                        diceRoll(value.c),
                        diceRoll(value.d),
                        diceRoll(value.e)
                      )
                      if (bettor.step != DefaultStep) {
                        val newBettor = Player.of(
                          bettor.combinationsDice,
                          bettor.round,
                          bettor.step + 1
                        )
                        val newPlayer = player - user
                        val updated = RoomState(
                          userRooms,
                          roomMembers,
                          newPlayer + (user -> newBettor)
                        )

                        (
                          updated,
                          sendToRoom(
                            room,
                            dice.asJson(Dice.encodeDice).toString()
                          )
                        )
                      } else
                        (
                          this,
                          Seq(
                            SendToUser(
                              user,
                              "You have spent all the rolls, choose a combination"
                            )
                          )
                        )
                    } else {

                      val calculation =
                        сalculationWeight(bettor, combinations, value)
                      if (calculation._2 != 0) {
                        if (bettor.round < 13) {
                          val combinationsDice = CombinationsDice.of(
                            calculation._1,
                            value,
                            calculation._2
                          )
                          val newBettor = Player.of(
                            bettor.combinationsDice :+ combinationsDice,
                            bettor.round + 1,
                            0
                          )
                          val newPlayer = player - user
                          val updated = RoomState(
                            userRooms,
                            roomMembers,
                            newPlayer + (user -> newBettor)
                          )

                          (
                            updated,
                            updated
                              .sendToRoom(
                                room,
                                Dice
                                  .of(
                                    diceRoll(0),
                                    diceRoll(0),
                                    diceRoll(0),
                                    diceRoll(0),
                                    diceRoll(0)
                                  )
                                  .asJson(Dice.encodeDice)
                                  .toString()
                              )
                          )
                        } else {
                          val combinationsDice = CombinationsDice.of(
                            calculation._1,
                            value,
                            calculation._2
                          )
                          val newBettor = Player.of(
                            bettor.combinationsDice :+ combinationsDice,
                            bettor.round,
                            0
                          )
                          val newPlayer = player - user
                          RoomState(userRooms, roomMembers, newPlayer)
                          val updated = RoomState(
                            userRooms,
                            roomMembers,
                            newPlayer + (user -> newBettor)
                          )

                          (
                            updated,
                            updated
                              .sendToRoom(
                                room,
                                resultGame(newPlayer + (user -> newBettor))
                              )
                          )
                        }
                      } else
                        (
                          this,
                          Seq(
                            SendToUser(
                              user,
                              "This combination is impossible! Choose a combination, please"
                            )
                          )
                        )
                    }
                  case Left(_) =>
                    (this, Seq(SendToUser(user, "What dice did you choose")))
                }
              case None =>
                (this, Seq(SendToUser(user, "Player initialization error")))
            }
          } else {
            (this, Seq(SendToUser(user, "It's another player's turn now")))
          }

        case None =>
          (this, Seq(SendToUser(user, "You are not currently in a room")))

      }

    case Disconnect(user) =>
      removeFromCurrentRoom(user)

    case InvalidInput(user, text) =>
      (this, Seq(SendToUser(user, s"Invalid input: $text")))
  }

  private def removeFromCurrentRoom(
      user: String
  ): (RoomState, Seq[OutputMessage]) = userRooms.get(user) match {
    case Some(room) =>
      val nextMembers = roomMembers.getOrElse(room, Set()) - user
      val nextState =
        if (nextMembers.isEmpty)
          RoomState(userRooms - user, roomMembers - room, player - user)
        else
          RoomState(
            userRooms - user,
            roomMembers + (room -> nextMembers),
            player - user
          )

      (nextState, sendToRoom(room, s"$user has left $room"))
    case None =>
      (this, Nil)
  }

  private def sendToRoom(room: String, text: String): Seq[OutputMessage] = {
    roomMembers
      .get(room)
      .map(SendToUsers(_, text))
      .toSeq
  }

  private def addToRoom(
      user: String,
      room: String
  ): (RoomState, Seq[OutputMessage]) = {
    val nextMembers = roomMembers.getOrElse(room, Set()) + user
    val nextState = RoomState(
      userRooms + (user -> room),
      roomMembers + (room -> nextMembers),
      player + (user -> DefaultPlayer)
    )

    (nextState, nextState.sendToRoom(room, s"$user has joined $room"))
  }

  private def sizeRoom(toRoom: String): Int = roomMembers
    .getOrElse(toRoom, Set())
    .toList
    .size
}

object RoomState {
  def apply(): RoomState = RoomState(Map.empty, Map.empty, Map.empty)
}
