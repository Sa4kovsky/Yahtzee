package game

import game.Room.{DefaultPlayer, sendToRoom}
import game.model.Combinations._
import game.model.Dice.decodeDice
import game.model.Player.{CombinationsDice, Player}
import game.model._
import io.circe.syntax.EncoderOps
import server.model.{OutputMessage, SendToUser}

import scala.util.Random

trait Game {
  def startGame(user: String, room: String, countPlayers: Int)(state: State): (State, Seq[OutputMessage])

  def game(dice: String, bettor: Player, room: String, user: String, combinations: String)(
    state: State
  ): (State, Seq[OutputMessage])
}

object YahtzeeGame extends Game {

  override def startGame(user: String, room: String, countPlayers: Int)(state: State): (State, Seq[OutputMessage]) = {
    val count = countUser(room)(state).size
    if (count == countPlayers) {
      val p = state.player.getOrElse(user, DefaultPlayer)
      if (p.round > 1 || p.step > 1)
        (state, sendToRoom(room, "The game began")(state.roomMembers))
      else
        (
          state,
          sendToRoom(
            room,
            Dice
              .of(diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None))
              .asJson(Dice.encodeDice)
              .toString()
          )(state.roomMembers)
        )
    } else (state, sendToRoom(room, "The room is designed for a larger number of people")(state.roomMembers))
  }

  private def countUser(room: String)(state: State): List[String] = state.roomMembers.getOrElse(room, List()).toList

  override def game(dice: String, bettor: Player, room: String, user: String, combinations: String)(
    state: State
  ): (State, Seq[OutputMessage]) =
    decodeDice(dice) match {
      case Some(value) =>
        if (
          value.a.isEmpty || value.b.isEmpty || value.c.isEmpty || value.d.isEmpty || value.e.isEmpty || combinations == Nothing.name
        )
          increaseStep(value, bettor, room, user)(state)
        else {
          increaseRound(value, bettor, room, user, combinations)(state)
        }
      case None => (state, Seq(SendToUser(user, "What dice did you choose")))
    }

  private def increaseStep(dice: Dice, bettor: Player, room: String, user: String)(
    state: State
  ): (State, Seq[OutputMessage]) = {
    val DefaultStep = 3

    val newDice = Dice.of(
      diceRoll(dice.a),
      diceRoll(dice.b),
      diceRoll(dice.c),
      diceRoll(dice.d),
      diceRoll(dice.e)
    )
    if (bettor.step != DefaultStep) {
      val newBettor = Player.of(bettor.combinationsDice, bettor.round, bettor.step + 1)

      val updated = State(state.userRooms, state.roomMembers, state.player + (user -> newBettor))

      (updated, sendToRoom(room, newDice.asJson(Dice.encodeDice).toString())(updated.roomMembers))
    } else
      (state, Seq(SendToUser(user, "You have spent all the rolls, choose a combination")))
  }

  private def diceRoll(x: Option[DiceSide]): Option[DiceSide] =
    x match {
      case Some(value) => Some(value)
      case None =>
        Random.nextInt(6) + 1 match {
          case 1 => Some(DiceSide.One)
          case 2 => Some(DiceSide.Two)
          case 3 => Some(DiceSide.Tree)
          case 4 => Some(DiceSide.Four)
          case 5 => Some(DiceSide.Five)
          case 6 => Some(DiceSide.Six)
          case _ => None
        }
    }

  private def increaseRound(dice: Dice, bettor: Player, room: String, user: String, combinations: String)(
    state: State
  ): (State, Seq[OutputMessage]) = {

    val calculation = calcWeight(bettor, combinations, dice)

    if (calculation._2 != 0) {
      if (bettor.round < 13) {
        val combinationsDice = CombinationsDice.of(calculation._1, dice, calculation._2)
        val newBettor        = Player.of(bettor.combinationsDice :+ combinationsDice, bettor.round + 1, 0)
        val newRoomMembers   = state.roomMembers.getOrElse(room, Set()) - user

        val updated = State(
          state.userRooms,
          state.roomMembers + (room -> (newRoomMembers + user)),
          state.player + (user      -> newBettor)
        )

        (
          updated,
          sendToRoom(
            room,
            Dice
              .of(diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None))
              .asJson(Dice.encodeDice)
              .toString()
          )(updated.roomMembers)
        )
      } else determinationResult(calculation._1, dice, calculation._2, bettor, room, user)(state)
    } else (state, Seq(SendToUser(user, "This combination is impossible! Choose a combination, please")))
  }

  private def calcWeight(player: Player, combinations: String, dice: Dice): (Combinations, Int) = {
    if (chekCombinations(player, combinations)) {
      val values = List(dice.a.get.value, dice.b.get.value, dice.c.get.value, dice.d.get.value, dice.e.get.value).sorted
      combinations match {
        case Ones.name   => (Ones, values.count(_ == 1))
        case Twos.name   => (Twos, values.count(_ == 2) * 2)
        case Threes.name => (Threes, values.count(_ == 3) * 3)
        case Fours.name  => (Fours, values.count(_ == 4) * 4)
        case Fives.name  => (Fives, values.count(_ == 5) * 5)
        case Sixes.name  => (Sixes, values.count(_ == 6) * 6)
        case ThreeOfAKind.name =>
          if (values.groupBy(identity).exists(_._2.length >= 3)) (ThreeOfAKind, values.sum) else (ThreeOfAKind, 0)
        case FourOfAKind.name =>
          if (values.groupBy(identity).exists(_._2.length >= 4)) (FourOfAKind, values.sum) else (FourOfAKind, 0)
        case FullHouse.name =>
          val grouped = values.groupBy(identity)
          if (grouped.exists(_._2.length == 3) && grouped.size == 2) (FullHouse, 25) else (FullHouse, 0)
        case SmallStraight.name =>
          if (
            values.containsSlice(Seq(1, 2, 3, 4))
            || values.containsSlice(Seq(2, 3, 4, 5))
            || values.containsSlice(Seq(3, 4, 5, 6))
          ) (SmallStraight, 30)
          else (SmallStraight, 0)
        case LargeStraight.name =>
          if (values == Seq(1, 2, 3, 4, 5) || values == Seq(2, 3, 4, 5, 6)) (LargeStraight, 40) else (LargeStraight, 0)
        case Yahtzee.name => if (values.distinct.size == 1) (Yahtzee, 50) else (Yahtzee, 0)
        case Chance.name  => (Chance, values.sum)
        case _            => (Nothing, 0)
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
    val combinationsDice = CombinationsDice.of(combinations, dice, weight)
    val newBettor        = Player.of(bettor.combinationsDice :+ combinationsDice, bettor.round + 1, 0)
    val newRoomMembers   = state.roomMembers.getOrElse(room, Set()) - user

    val updated = State(
      state.userRooms,
      state.roomMembers + (room -> (newRoomMembers + user)),
      state.player + (user      -> newBettor)
    )

    val newPlayer = state.player - user
    (updated, sendToRoom(room, resultGame(newPlayer + (user -> newBettor), room)(state))(updated.roomMembers))
  }

  private def resultGame(player: Map[String, Player], room: String)(state: State): String = {
    val listPlayer = countUser(room)(state)
    val size       = listPlayer.count(player.getOrElse(_, Player.of()).round == 14)
    if (listPlayer.size == size) {
      val result: Map[String, Int] = player.flatMap(x => amountWeight(x._1, x._2))
      result.tail.max.toString()
    } else
      "Not all"
  }

  private def amountWeight(key: String, player: Player): Map[String, Int] = {
    val c = player.combinationsDice.foldLeft(0)((x, player) => x + player.weight)
    Map(key -> c)
  }
}
