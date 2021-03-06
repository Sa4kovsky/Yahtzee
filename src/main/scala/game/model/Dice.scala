package game.model

import io.circe._

sealed trait DiceSide { val value: Int }

object DiceSide {
  case object One  extends DiceSide { val value = 1 }
  case object Two  extends DiceSide { val value = 2 }
  case object Tree extends DiceSide { val value = 3 }
  case object Four extends DiceSide { val value = 4 }
  case object Five extends DiceSide { val value = 5 }
  case object Six  extends DiceSide { val value = 6 }

  implicit val decodeDiceSide: Decoder[Option[DiceSide]] = Decoder[String].emap { s =>
    s.toIntOption match {
      case Some(1) => Right(Some(One))
      case Some(2) => Right(Some(Two))
      case Some(3) => Right(Some(Tree))
      case Some(4) => Right(Some(Four))
      case Some(5) => Right(Some(Five))
      case Some(6) => Right(Some(Six))
      case _       => Right(None)
    }
  }

  implicit val encoderDiceSide: Encoder[DiceSide] = Encoder[String].contramap(_.value.toString)
}

sealed abstract case class Dice private (
  a: Option[DiceSide],
  b: Option[DiceSide],
  c: Option[DiceSide],
  d: Option[DiceSide],
  e: Option[DiceSide]
)

object Dice {
  val Default: Dice = new Dice(None, None, None, None, None) {}

  def of(
    a: Option[DiceSide],
    b: Option[DiceSide],
    c: Option[DiceSide],
    d: Option[DiceSide],
    e: Option[DiceSide]
  ): Dice =
    new Dice(a, b, c, d, e) {}

  implicit val decodeDice: Decoder[Dice] =
    Decoder.forProduct5("a", "b", "c", "d", "e")(Dice.of)

  implicit val encodeDice: Encoder[Dice] =
    Encoder.forProduct5("a", "b", "c", "d", "e")(u => (u.a, u.b, u.c, u.d, u.e))

  def decodeDice(dice: String): Option[Dice] =
    jawn.decode(dice)(Dice.decodeDice) match {
      case Right(value) => Some(value)
      case Left(_)      => None
    }
}
