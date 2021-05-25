package game

import _root_.game.model.Combinations.SimpleCombinations._
import _root_.game.model.{Dice, Room, User}
import _root_.game.model.DiceSide.{One, Six, Tree, Two}
import _root_.game.model.Player.{CombinationsDice, Player}
import org.scalatest.funsuite.AnyFunSuite
import server.model.Message.GameErrorMessage.StartGame
import server.model.Message.RoomErrorMessage.SizeRoom

class GameTest extends AnyFunSuite {

  val combinations = List[CombinationsDice](
    CombinationsDice.of(Ones, Dice.of(Some(One), Some(One), Some(One), Some(Two), Some(Six)), 3),
    CombinationsDice.of(Sixes, Dice.of(Some(Six), Some(One), Some(Six), Some(Two), Some(Tree)), 12)
  )
  val player = Player.of(combinations, round = 2, step = 1)
  val game =
    GameState(Map(User("Test") -> Room("Game")), Map(Room("Game") -> Set(User("Test"))), Map(User("Test") -> player))

  val player2 = Player.of(combinations, round = 2, step = 1)
  val game2 =
    GameState(
      Map(User("Test") -> Room("Game"), User("Test2") -> Room("Game")),
      Map(Room("Game") -> Set(User("Test"), User("Test2"))),
      Map(User("Test") -> player, User("Test2")       -> player)
    )

  test("test - starting with an incomplete room") {
    val pars = Game.startGame(User("Test"), Room("Game"), 2)(game)
    assert(SizeRoom.text == pars._2.head.toString)
  }

  test("test - starting game when game start") {
    val pars = Game.startGame(User("Test2"), Room("Game"), 2)(game2)
    assert(StartGame.text == pars._2.head.toString)
  }

  test("test - checking combinations and determining the weight of game dice") {
    val pars = Game.calcWeight(player, Twos, Dice.of(Some(Two), Some(One), Some(Two), Some(Two), Some(Tree)))
    assert((Twos, 6) == pars)
  }

  test("test - error checking combinations and determining the weight of game dice") {
    val pars = Game.calcWeight(player, Ones, Dice.of(Some(Two), Some(One), Some(Two), Some(Two), Some(Tree)))
    assert((Nothing, 0) == pars)
  }
}
