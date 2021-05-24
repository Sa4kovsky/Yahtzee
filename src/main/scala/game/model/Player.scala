package game.model

import game.model.Combinations.Nothing

object Player {

  private type Weight = Int
  private type Rounds = Int
  private type Step   = Int

  sealed abstract case class CombinationsDice private (
    combinations: Combinations,
    dice: Dice,
    weight: Weight
  )

  sealed abstract case class Player private (
    combinationsDice: List[CombinationsDice],
    round: Rounds,
    step: Step
  )

  object CombinationsDice {
    val Default = new CombinationsDice(Nothing, Dice.Default, 0) {}
    def of(
      combinations: Combinations,
      dice: Dice,
      weight: Int
    ): CombinationsDice =
      new CombinationsDice(combinations, dice, weight) {}
  }

  object Player {
    val Default: Player = new Player(List(), 1, 1) {}
    def of(
      combinationsDice: List[CombinationsDice],
      round: Rounds,
      step: Step
    ): Player =
      new Player(combinationsDice, round, step) {}
  }

}
