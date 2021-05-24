package game

import cats.Monad
import cats.mtl.{Stateful, Tell}
import cats.syntax.all._
import game.Room._
import game.model.Combinations._
import game.model.Player.{CombinationsDice, Player}
import game.model._
import io.circe.syntax.EncoderOps
import server.model._

import scala.util.Random

trait Game[F[_]] {

  type User        = String
  type Room        = String
  type Count       = Int
  type Combination = String

  def startGame(user: User, room: Room, countPlayers: Count): F[Unit]
  def game(dice: String, bettor: Player, room: Room, user: User, combinations: Combination): F[Unit]
}

object Game {

  def make[F[_]: Monad: Stateful[*[_], State]: Tell[*[_], Seq[OutputMessage]]]: Game[F] = new Game[F] {

    override def startGame(user: User, room: Room, countPlayers: Count): F[Unit] = for {
      state <- Stateful.get[F, State]
      count  = countUser(room)(state).size
      _ <-
        if (count == countPlayers) {
          val p = state.player.getOrElse(user, DefaultPlayer)
          if (p.round > 1 || p.step > 1)
            Room.make.sendToRoom(room, Message.GameErrorMessage.StartGame.text)
          else
            Room.make.sendToRoom(
              room,
              Dice
                .of(diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None))
                .asJson(Dice.encodeDice)
                .toString()
            )
        } else Room.make.sendToRoom(room, Message.RoomErrorMessage.SizeRoom.text)
    } yield ()

    override def game(dice: String, bettor: Player, room: Room, user: User, combinations: Combination): F[Unit] = for {
      state <- Stateful.get[F, State]
      _ <- Dice.decodeDice(dice) match {
        case Some(value) =>
          if (
            value.a.isEmpty || value.b.isEmpty || value.c.isEmpty || value.d.isEmpty || value.e.isEmpty || combinations == Nothing.name
          )
            increaseStep(value, bettor, room, user)(state)
          else {
            increaseRound(value, bettor, room, user, combinations)(state)
          }
        case None => increaseRound(Dice.of(), bettor, room, user, combinations)(state)
      }
    } yield ()

    private def increaseStep(dice: Dice, bettor: Player, room: Room, user: User)(state: State): F[Unit] = {
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
        val updated   = State(state.userRooms, state.roomMembers, state.player + (user -> newBettor))

        Stateful[F, State].set(updated) *> Room.make.sendToRoom(room, newDice.asJson(Dice.encodeDice).toString)
      } else
        Tell[F, Seq[OutputMessage]].tell(Seq(SendToUser(user, Message.GameErrorMessage.RollDice.text)))
    }

    private def increaseRound(dice: Dice, bettor: Player, room: Room, user: User, combinations: Combination)(
      state: State
    ): F[Unit] = {

      val calculation = calcWeight(bettor, combinations, dice)

      if (calculation._1 != Nothing) {
        if (bettor.round < 13) {
          val combinationsDice = CombinationsDice.of(calculation._1, dice, calculation._2)
          val newBettor        = Player.of(bettor.combinationsDice :+ combinationsDice, bettor.round + 1, 0)
          val newRoomMembers   = state.roomMembers.getOrElse(room, Set()) - user

          val updated = State(
            state.userRooms,
            state.roomMembers + (room -> (newRoomMembers + user)),
            state.player + (user      -> newBettor)
          )

          Stateful[F, State].set(updated) *> Room.make.sendToRoom(
            room,
            Dice
              .of(diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None), diceRoll(None))
              .asJson(Dice.encodeDice)
              .toString
          )
        } else determinationResult(calculation._1, dice, calculation._2, bettor, room, user)(state)
      } else
        Tell[F, Seq[OutputMessage]].tell(Seq(SendToUser(user, Message.GameErrorMessage.ImpossibleCombination.text)))
    }

    private def determinationResult(
      combinations: Combinations,
      dice: Dice,
      weight: Int,
      bettor: Player,
      room: Room,
      user: User
    )(state: State): F[Unit] = {
      val combinationsDice = CombinationsDice.of(combinations, dice, weight)
      val newBettor        = Player.of(bettor.combinationsDice :+ combinationsDice, bettor.round + 1, 0)
      val newRoomMembers   = state.roomMembers.getOrElse(room, Set()) - user

      val updated = State(
        state.userRooms,
        state.roomMembers + (room -> (newRoomMembers + user)),
        state.player + (user      -> newBettor)
      )

      val newPlayer = state.player - user
      Stateful[F, State]
        .set(updated) *> Room.make.sendToRoom(room, resultGame(newPlayer + (user -> newBettor), room)(state))
    }

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

  private def calcWeight(player: Player, combinations: String, dice: Dice): (Combinations, Int) = {
    if (chekCombinations(player, combinations))
      if (checkDice(dice).isDefined) {
        val values =
          List(dice.a.get.value, dice.b.get.value, dice.c.get.value, dice.d.get.value, dice.e.get.value).sorted
        parsCombination(combinations) match {
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
      } else (parsCombination(combinations), 0)
    else (Nothing, 0)
  }

  private def checkDice(dice: Dice): Option[Dice] =
    if (dice.a.isEmpty || dice.a.isEmpty || dice.a.isEmpty || dice.a.isEmpty || dice.a.isEmpty) None
    else Some(dice)

  private def chekCombinations(players: Player, combinations: String): Boolean =
    !players.combinationsDice.exists(x => x.combinations.name == combinations)

  private def parsCombination(combinations: String): Combinations = combinations match {
    case Ones.name          => Ones
    case Twos.name          => Twos
    case Threes.name        => Threes
    case Fours.name         => Fours
    case Fives.name         => Fives
    case Sixes.name         => Sixes
    case ThreeOfAKind.name  => ThreeOfAKind
    case FourOfAKind.name   => FourOfAKind
    case FullHouse.name     => FullHouse
    case SmallStraight.name => SmallStraight
    case LargeStraight.name => LargeStraight
    case Yahtzee.name       => Yahtzee
    case Chance.name        => Chance
    case _                  => Nothing
  }

  private def resultGame(player: Map[String, Player], room: String)(state: State): String = {
    val listPlayer = countUser(room)(state)
    val size       = listPlayer.count(player.getOrElse(_, Player.of()).round == 14)
    if (listPlayer.size == size) {
      val result: Map[String, Int] = player.flatMap(x => amountWeight(x._1, x._2))
      result.tail.max.toString()
    } else
      Message.GameErrorMessage.NotFinished.text
  }

  private def countUser(room: String)(state: State): List[String] =
    state.roomMembers.getOrElse(room, List()).toList

  private def amountWeight(key: String, player: Player): Map[String, Int] = {
    val c = player.combinationsDice.foldLeft(0)((x, player) => x + player.weight)
    Map(key -> c)
  }
}
