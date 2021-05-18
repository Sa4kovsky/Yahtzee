package game

import game.Room.sendToRoom
import game.model.Player.{CombinationsDice, Player}
import game.model._
import io.circe.syntax.EncoderOps
import server.model.{OutputMessage, SendToUser}

import scala.util.Random

trait Game {
  def diceRoll(x: Int): Int

  def increaseStep(
      dice: Dice,
      bettor: Player,
      room: String,
      user: String
  )(state: State): (State, Seq[OutputMessage])

  def increaseRound(
      dice: Dice,
      bettor: Player,
      room: String,
      user: String,
      combinations: String
  )(state: State): (State, Seq[OutputMessage])

}

object YahtzeeGame extends Game {

  override def increaseStep(
      dice: Dice,
      bettor: Player,
      room: String,
      user: String
  )(state: State): (State, Seq[OutputMessage]) = { //step
    val DefaultStep = 3

    val newDice = Dice.of(
      diceRoll(dice.a),
      diceRoll(dice.b),
      diceRoll(dice.c),
      diceRoll(dice.d),
      diceRoll(dice.e)
    )
    if (bettor.step != DefaultStep) {
      val newBettor = Player.of(
        bettor.combinationsDice,
        bettor.round,
        bettor.step + 1
      )

      val updated = State(
        state.userRooms,
        state.roomMembers,
        state.player + (user -> newBettor)
      )

      (
        updated,
        sendToRoom(
          room,
          newDice.asJson(Dice.encodeDice).toString()
        )(updated.roomMembers)
      )
    } else
      (
        state,
        Seq(
          SendToUser(
            user,
            "You have spent all the rolls, choose a combination"
          )
        )
      )
  }

  override def increaseRound(
      dice: Dice,
      bettor: Player,
      room: String,
      user: String,
      combinations: String
  )(state: State): (State, Seq[OutputMessage]) = {
    //Round ++
    val calculation =
      calcWeight(bettor, combinations, dice)

    if (calculation._2 != 0) {
      if (bettor.round < 13) {
        val combinationsDice = CombinationsDice.of(
          calculation._1,
          dice,
          calculation._2
        )
        val newBettor = Player.of(
          bettor.combinationsDice :+ combinationsDice,
          bettor.round + 1,
          0
        )
        val newRoomMembers =
          state.roomMembers.getOrElse(room, Set()) - user

        // val newPlayer = player - user
        val updated = State(
          state.userRooms,
          state.roomMembers + (room -> (newRoomMembers + user)),
          state.player + (user -> newBettor)
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
        determinationResult(
          calculation._1,
          dice,
          calculation._2,
          bettor,
          room,
          user
        )(state)
      }
    } else
      (
        state,
        Seq(
          SendToUser(
            user,
            "This combination is impossible! Choose a combination, please"
          )
        )
      )
  }

  override def diceRoll(x: Int): Int = if (x == 0) Random.nextInt(6) + 1 else x

  private def calcWeight(
      player: Player,
      combinations: String,
      dice: Dice
  ): (Combinations, Int) = {
    if (chekCombinations(player, combinations)) {
      val values = List(dice.a, dice.b, dice.c, dice.d, dice.e).sorted
      combinations match {
        case Ones.name   => (Ones, values.count(_ == 1))
        case Twos.name   => (Twos, values.count(_ == 2) * 2)
        case Threes.name => (Threes, values.count(_ == 3) * 3)
        case Fours.name  => (Fours, values.count(_ == 4) * 4)
        case Fives.name  => (Fives, values.count(_ == 5) * 5)
        case Sixes.name  => (Sixes, values.count(_ == 6) * 6)
        case ThreeOfAKind.name =>
          if (values.groupBy(identity).exists(_._2.length >= 3))
            (ThreeOfAKind, values.sum)
          else (ThreeOfAKind, 0)
        case FourOfAKind.name =>
          if (values.groupBy(identity).exists(_._2.length >= 4))
            (FourOfAKind, values.sum)
          else (FourOfAKind, 0)
        case FullHouse.name =>
          val grouped = values.groupBy(identity)
          if (grouped.exists(_._2.length == 3) && grouped.size == 2)
            (FullHouse, 25)
          else (FullHouse, 0)
        case SmallStraight.name =>
          if (
            values.containsSlice(Seq(1, 2, 3, 4))
            || values.containsSlice(Seq(2, 3, 4, 5))
            || values.containsSlice(Seq(3, 4, 5, 6))
          )
            (SmallStraight, 30)
          else (SmallStraight, 0)
        case LargeStraight.name =>
          if (values == Seq(1, 2, 3, 4, 5) || values == Seq(2, 3, 4, 5, 6))
            (LargeStraight, 40)
          else (LargeStraight, 0)
        case Yahtzee.name =>
          if (values.distinct.size == 1)
            (Yahtzee, 50)
          else
            (Yahtzee, 0)
        case Chance.name => (Chance, values.sum)
        case _           => (Nothing, 0)
      }
    } else (Nothing, 0)
  }

  private def chekCombinations(players: Player, combinations: String): Boolean =
    !players.combinationsDice.exists(x => x.combinations.name == combinations)

  private def determinationResult(
      combinations: Combinations,
      dice: Dice,
      weight: Int,
      bettor: Player,
      room: String,
      user: String
  )(state: State): (State, Seq[OutputMessage]) = {
    val combinationsDice = CombinationsDice.of(
      combinations,
      dice,
      weight
    )
    val newBettor = Player.of(
      bettor.combinationsDice :+ combinationsDice,
      bettor.round,
      0
    )
    val updated = State(
      state.userRooms,
      state.roomMembers,
      state.player + (user -> newBettor)
    )

    val newPlayer = state.player - user
    (
      updated,
      sendToRoom(
        room,
        resultGame(newPlayer + (user -> newBettor))
      )(updated.roomMembers)
    )
  }

  private def resultGame(player: Map[String, Player]): String = {
    val countFinish = player.count(_._2.round == 13)
    if (countFinish == player.size) {
      val result: Map[String, Int] =
        player.flatMap(x => amountWeight(x._1, x._2))
      result.tail.max.toString()
    } else
      "Not all"
  }

  private def amountWeight(key: String, player: Player): Map[String, Int] = {
    val c =
      player.combinationsDice.foldLeft(0)((x, player) => x + player.weight)
    Map(key -> c)
  }
}
