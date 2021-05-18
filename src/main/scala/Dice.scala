import io.circe._

sealed abstract case class Dice private (a: Int, b: Int, c: Int, d: Int, e: Int)
object Dice {
  def of(a: Int = 0, b: Int = 0, c: Int = 0, d: Int = 0, e: Int = 0): Dice =
    new Dice(a, b, c, d, e) {}

  implicit val decodeDice: Decoder[Dice] =
    Decoder.forProduct5("a", "b", "c", "d", "e")(Dice.of)

  implicit val encodeDice: Encoder[Dice] =
    Encoder.forProduct5("a", "b", "c", "d", "e")(u => (u.a, u.b, u.c, u.d, u.e))
}
