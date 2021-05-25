package game

import game.Roll.diceRoll
import game.model.Combinations.ComplexCombinations._
import game.model.Combinations.SimpleCombinations._
import game.model.Player.{CombinationsDice, Player}
import game.model._
import io.circe.syntax.EncoderOps
import server.model._

trait Game {
  def startGame(user: User, room: Room, countPlayers: Int)(implicit state: GameState): (GameState, Seq[OutputMessage])

  def game(dice: Option[Dice], bettor: Player, room: Room, user: User, combinations: Combinations)(implicit
    state: GameState
  ): (GameState, Seq[OutputMessage])
}

object Game extends Game {

  override def startGame(user: User, room: Room, countPlayers: Int)(implicit
    state: GameState
  ): (GameState, Seq[OutputMessage]) = {
    val count = countUser(room).size
    if (count == countPlayers) {
      val p = state.player.getOrElse(user, Player.Default)
      if (p.round > 1 || p.step > 1)
        (
          state,
          Seq(
            SendToUsers(state.roomMembers.getOrElse(room, Set.empty), Message.GameErrorMessage.StartGame.text)
          )
        )
      else
        (
          state,
          Seq(
            SendToUsers(
              state.roomMembers.getOrElse(room, Set.empty),
              Dice
                .of(diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None))
                .asJson(Dice.encodeDice)
                .toString
            )
          )
        )

    } else
      (
        state,
        Seq(
          SendToUsers(state.roomMembers.getOrElse(room, Set.empty), Message.RoomErrorMessage.SizeRoom.text)
        )
      )
  }

  override def game(dice: Option[Dice], bettor: Player, room: Room, user: User, combinations: Combinations)(implicit
    state: GameState
  ): (GameState, Seq[OutputMessage]) =
    dice match {
      case Some(value) =>
        if (
          value.a.isEmpty || value.b.isEmpty || value.c.isEmpty || value.d.isEmpty || value.e.isEmpty || combinations == Nothing
        )
          increaseStep(value, bettor, room, user)
        else {
          increaseRound(value, bettor, room, user, combinations)
        }
      case None => increaseRound(Dice.Default, bettor, room, user, combinations)
    }

  private def increaseStep(dice: Dice, bettor: Player, room: Room, user: User)(implicit
    state: GameState
  ): (GameState, Seq[OutputMessage]) = {
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

      val updated = GameState(state.userRooms, state.roomMembers, state.player + (user -> newBettor))

      (
        updated,
        Seq(
          SendToUsers(updated.roomMembers.getOrElse(room, Set.empty), newDice.asJson(Dice.encodeDice).toString)
        )
      )
    } else
      (state, Seq(SendToUser(user, Message.GameErrorMessage.RollDice.text)))
  }

  private def increaseRound(dice: Dice, bettor: Player, room: Room, user: User, combinations: Combinations)(implicit
    state: GameState
  ): (GameState, Seq[OutputMessage]) = {

    val calculation = calcWeight(bettor, combinations, dice)

    if (calculation._1 != Nothing & calculation._2 != 0) {
      if (bettor.round < 13) {
        val combinationsDice = CombinationsDice.of(calculation._1, dice, calculation._2)
        val newBettor        = Player.of(bettor.combinationsDice :+ combinationsDice, bettor.round + 1, 0)
        val newRoomMembers   = state.roomMembers.getOrElse(room, Set()) - user

        val updated = GameState(
          state.userRooms,
          state.roomMembers + (room -> (newRoomMembers + user)),
          state.player + (user      -> newBettor)
        )

        (
          updated,
          Seq(
            SendToUsers(
              updated.roomMembers.getOrElse(room, Set.empty),
              Dice
                .of(diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None))
                .asJson(Dice.encodeDice)
                .toString
            )
          )
        )
      } else determinationResult(calculation._1, dice, calculation._2, bettor, room, user)(state)
    } else (state, Seq(SendToUser(user, Message.GameErrorMessage.ImpossibleCombination.text)))
  }

  private[game] def calcWeight(player: Player, combinations: Combinations, dice: Dice): (Combinations, Int) = {
    if (chekCombinations(player, combinations))
      if (checkDice(dice).isDefined) {
        val values =
          List(dice.a.get.value, dice.b.get.value, dice.c.get.value, dice.d.get.value, dice.e.get.value).sorted
        combinations match {
          case Ones   => (Ones, values.count(_ == 1))
          case Twos   => (Twos, values.count(_ == 2) * 2)
          case Threes => (Threes, values.count(_ == 3) * 3)
          case Fours  => (Fours, values.count(_ == 4) * 4)
          case Fives  => (Fives, values.count(_ == 5) * 5)
          case Sixes  => (Sixes, values.count(_ == 6) * 6)
          case ThreeOfAKind =>
            if (values.groupBy(identity).exists(_._2.length >= 3)) (ThreeOfAKind, values.sum) else (ThreeOfAKind, 0)
          case FourOfAKind =>
            if (values.groupBy(identity).exists(_._2.length >= 4)) (FourOfAKind, values.sum) else (FourOfAKind, 0)
          case FullHouse =>
            val grouped = values.groupBy(identity)
            if (grouped.exists(_._2.length == 3) && grouped.size == 2) (FullHouse, 25) else (FullHouse, 0)
          case SmallStraight =>
            if (
              values.containsSlice(Seq(1, 2, 3, 4))
              || values.containsSlice(Seq(2, 3, 4, 5))
              || values.containsSlice(Seq(3, 4, 5, 6))
            ) (SmallStraight, 30)
            else (SmallStraight, 0)
          case LargeStraight =>
            if (values == Seq(1, 2, 3, 4, 5) || values == Seq(2, 3, 4, 5, 6)) (LargeStraight, 40)
            else (LargeStraight, 0)
          case Yahtzee => if (values.distinct.size == 1) (Yahtzee, 50) else (Yahtzee, 0)
          case Chance  => (Chance, values.sum)
          case Nothing => (Nothing, 0)
        }
      } else (combinations, -1)
    else (Nothing, 0)
  }

  private def checkDice(dice: Dice): Option[Dice] =
    if (dice.a.isEmpty || dice.a.isEmpty || dice.a.isEmpty || dice.a.isEmpty || dice.a.isEmpty) None
    else Some(dice)

  private def chekCombinations(players: Player, combinations: Combinations): Boolean =
    !players.combinationsDice.exists(x => x.combinations == combinations)

  private def determinationResult(
    combinations: Combinations,
    dice: Dice,
    weight: Int,
    bettor: Player,
    room: Room,
    user: User
  )(implicit state: GameState): (GameState, Seq[OutputMessage]) = {
    val combinationsDice = CombinationsDice.of(combinations, dice, weight)
    val newBettor        = Player.of(bettor.combinationsDice :+ combinationsDice, bettor.round + 1, 0)
    val newRoomMembers   = state.roomMembers.getOrElse(room, Set()) - user

    val updated = GameState(
      state.userRooms,
      state.roomMembers + (room -> (newRoomMembers + user)),
      state.player + (user      -> newBettor)
    )

    val newPlayer = state.player - user
    (
      updated,
      Seq(
        SendToUsers(
          updated.roomMembers.getOrElse(room, Set.empty),
          resultGame(newPlayer + (user -> newBettor), room)
        )
      )
    )
  }

  private def resultGame(player: Map[User, Player], room: Room)(implicit state: GameState): String = {
    val listPlayer = countUser(room)(state)
    val size       = listPlayer.count(player.getOrElse(_, Player.Default).round == 14)
    if (listPlayer.size == size) {
      val result: Map[User, Int] = player.flatMap(x => amountWeight(x._1, x._2))
      result.tail.maxBy(_._2).toString()
    } else
      Message.GameErrorMessage.NotFinished.text
  }

  private def countUser(room: Room)(implicit state: GameState): List[User] =
    state.roomMembers.getOrElse(room, List()).toList

  private def amountWeight(key: User, player: Player): Map[User, Int] = {
    val simple = player.combinationsDice
      .filter(_.combinations.isInstanceOf[SimpleCombinations])
      .foldLeft(0)((x, player) => x + player.weight)
    val complex = player.combinationsDice
      .filter(_.combinations.isInstanceOf[ComplexCombinations])
      .foldLeft(0)((x, player) => x + player.weight)
    if (simple > 63)
      Map(key    -> (simple + complex + 35))
    else Map(key -> (simple + complex))
  }
}
