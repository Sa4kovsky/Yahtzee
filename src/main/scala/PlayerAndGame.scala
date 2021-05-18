import java.util.UUID

object PlayerAndGame {

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
        combinations: Combinations,
        dice: Dice,
        weight: Int = 0
    ): CombinationsDice =
      new CombinationsDice(combinations, dice, weight) {}
  }

  object Player {
    def of(
        combinationsDice: List[CombinationsDice],
        round: Rounds = 0,
        step: Step = 0
    ): Player =
      new Player(combinationsDice, round, step) {}
  }

}
