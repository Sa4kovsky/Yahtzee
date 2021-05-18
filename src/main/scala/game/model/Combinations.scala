package game.model

trait Combinations {
  val name: String
}
trait SimpleCombinations extends Combinations
trait ComplexCombinations extends Combinations

case object Nothing extends SimpleCombinations { val name = "Nothing" }
case object Ones extends SimpleCombinations { val name = "Ones" }
case object Twos extends SimpleCombinations { val name = "Twos" }
case object Threes extends SimpleCombinations { val name = "Threes" }
case object Fours extends SimpleCombinations { val name = "Fours" }
case object Fives extends SimpleCombinations { val name = "Fives" }
case object Sixes extends SimpleCombinations { val name = "Sixes" }

case object ThreeOfAKind extends ComplexCombinations {
  val name = "ThreeOfAKind"
}
case object FourOfAKind extends ComplexCombinations { val name = "FourOfAKind" }
case object FullHouse extends ComplexCombinations { val name = "FullHouse" }
case object SmallStraight extends ComplexCombinations {
  val name = "SmallStraight"
}
case object LargeStraight extends ComplexCombinations {
  val name = "LargeStraight"
}
case object Yahtzee extends ComplexCombinations { val name = "Yahtzee" }
case object Chance extends ComplexCombinations { val name = "Chance" }
