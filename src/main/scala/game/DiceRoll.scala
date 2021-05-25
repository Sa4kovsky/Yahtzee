package game

import game.model.DiceSide

import scala.util.Random

sealed trait Roll {
  def diceRoll(x: Option[DiceSide]): Option[DiceSide]
}

object Roll extends Roll {
  override def diceRoll(x: Option[DiceSide]): Option[DiceSide] =
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
}
