import Player.Player

import scala.util.Random

trait Game {
  def diceRoll(x: Int): Int

  def сalculationWeight(
      player: Player,
      combinations: String,
      dice: Dice
  ): (Combinations, Int)

  def resultGame(player: Map[String, Player]): String

}

object YahtzeeGame extends Game {

  override def diceRoll(x: Int): Int = if (x == 0) Random.nextInt(6) + 1 else x

  override def сalculationWeight(
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
    players.combinationsDice
      .find(x => x.combinations.name == combinations)
      .isEmpty

  override def resultGame(player: Map[String, Player]): String = {
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
