package game

import game.Room.{addToRoom, removeFromCurrentRoom, sendToRoom, sizeRoom}
import game.YahtzeeGame.{diceRoll, resultGame, сalculationWeight}
import game.model.Player._
import game.model.{Dice, Nothing}
import io.circe.jawn
import io.circe.syntax.EncoderOps
import server.model._

case class State(
    userRooms: Map[String, String],
    roomMembers: Map[String, Set[String]],
    player: Map[String, Player]
) {
  val DefaultCountPlayerInRoom = 2

  val DefaultStep = 3

  def process(msg: InputMessage): (State, Seq[OutputMessage]) = msg match {
    case Help(user) =>
      (this, Seq(SendToUser(user, InputMessage.HelpText)))

    case Chat(user, text) =>
      userRooms.get(user) match {
        case Some(room) =>
          (this, sendToRoom(room, s"$user: $text")(this.roomMembers))

        case None =>
          (this, Seq(SendToUser(user, "You are not currently in a room")))
      }

    case EnterRoom(user, toRoom) =>
      userRooms.get(user) match {
        case None =>
          // Первый раз - добро пожаловать
          val (finalState, enterMessages) =
            addToRoom(user, toRoom)(this)

          (finalState, Seq(WelcomeUser(user)) ++ enterMessages)

        case Some(currentRoom) if currentRoom == toRoom =>
          (this, Seq(SendToUser(user, "You are already in that room!")))

        case Some(_) =>
          if (sizeRoom(toRoom)(roomMembers) >= DefaultCountPlayerInRoom)
            (this, Seq(SendToUser(user, "The room is occupied")))
          else {

            val (intermediateState, leaveMessages) =
              removeFromCurrentRoom(user)(
                State(userRooms, roomMembers, player)
              )
            val (finalState, enterMessages) =
              addToRoom(user, toRoom)(intermediateState)

            (finalState, leaveMessages ++ enterMessages)
          }
      }

    case ListRooms(user) =>
      val roomList = roomMembers.keys.toList.sorted
        .mkString("Rooms:\n\t", "\n\t", "")

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
          "You are not currently in a room"
      }
      (this, Seq(SendToUser(user, memberList)))

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
              )(this.roomMembers)
            )
          } else
            (
              this,
              sendToRoom(room, "Choose or create another room")(
                this.roomMembers
              )
            )
        case None =>
          (this, Seq(SendToUser(user, "You are not currently in a room")))
      }

    case Round(user, dice, combinations) =>
      userRooms.get(user) match {
        case Some(room) =>
          val rooms: List[String] = roomMembers.getOrElse(room, List()).toList
          if (user == rooms.head) {
            player.get(user) match {
              case Some(bettor) =>
                jawn.decode(dice)(Dice.decodeDice) match {
                  case Right(value) =>
                    if (
                      value.a == 0 || value.b == 0 || value.c == 0 || value.d == 0 || value.e == 0 || combinations == Nothing.name
                    ) {
                      //step
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

                        val updated = State(
                          userRooms,
                          roomMembers,
                          player + (user -> newBettor)
                        )

                        (
                          updated,
                          sendToRoom(
                            room,
                            dice.asJson(Dice.encodeDice).toString()
                          )(updated.roomMembers)
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
                      //Round ++
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
                          val newRoomMembers =
                            roomMembers.getOrElse(room, Set()) - user

                          // val newPlayer = player - user
                          val updated = State(
                            userRooms,
                            roomMembers + (room -> (newRoomMembers + user)),
                            player + (user -> newBettor)
                          )

                          (
                            updated,
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
                            )(updated.roomMembers)
                          )
                        } else {
                          //result
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
                          val updated = State(
                            userRooms,
                            roomMembers,
                            player + (user -> newBettor)
                          )

                          val newPlayer = player - user
                          (
                            updated,
                            sendToRoom(
                              room,
                              resultGame(newPlayer + (user -> newBettor))
                            )(updated.roomMembers)
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
      removeFromCurrentRoom(user)(State(userRooms, roomMembers, player))

    case InvalidInput(user, text) =>
      (this, Seq(SendToUser(user, s"Invalid input: $text")))
  }

}

object State {
  def apply(): State = State(Map.empty, Map.empty, Map.empty)
}