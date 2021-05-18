package game.model

object Player {

  private type Weight = Int
  private type Rounds = Int
  private type Step = Int

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

    def of(
        combinations: Combinations = Nothing,
        dice: Dice = Dice.of(),
        weight: Int = 0
    ): CombinationsDice =
      new CombinationsDice(combinations, dice, weight) {}
  }

  object Player {

    def of(
        combinationsDice: List[CombinationsDice],
        round: Rounds = 1,
        step: Step = 1
    ): Player =
      new Player(combinationsDice, round, step) {}
  }

}
