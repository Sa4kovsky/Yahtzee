package game

import game.model.Combinations
import game.model.Combinations.ComplexCombinations._
import game.model.Combinations.SimpleCombinations._

sealed trait Parsing {
  def parsCombination(combinations: String): Combinations
}

case object Parsing extends Parsing {
  override def parsCombination(combinations: String): Combinations = combinations match {
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
}
