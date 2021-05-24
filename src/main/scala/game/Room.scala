package game

import cats.Monad
import cats.mtl.{Stateful, Tell}
import cats.syntax.all._
import game.model.Player.Player
import server.model._

trait Room[F[_]] {

  type Room = String
  type User = String

  def removeFromCurrentRoom(user: User): F[Unit]
  def sendToRoom(room: Room, text: String): F[Unit]
  def addToRoom(room: Room, user: User): F[Unit]
  def roomSize(room: Room): F[Int]
}

object Room {

  val DefaultPlayer: Player = Player.of()

  def make[F[_]: Monad: Stateful[*[_], State]: Tell[*[_], Seq[OutputMessage]]]: Room[F] = new Room[F] {

    override def removeFromCurrentRoom(user: Room): F[Unit] = for {
      state <- Stateful.get[F, State]
      _ <- state.userRooms.get(user) match {
        case Some(room) =>
          val nextMembers = state.roomMembers.getOrElse(room, Set()) - user
          val nextState =
            if (nextMembers.isEmpty)
              State(state.userRooms - user, state.roomMembers - room, state.player - user)
            else
              State(state.userRooms - user, state.roomMembers + (room -> nextMembers), state.player - user)
          Stateful[F, State].set(nextState) *> sendToRoom(room, s"$user has left $room")
        case None => Monad[F].unit
      }
    } yield ()

    override def sendToRoom(room: Room, text: String): F[Unit] = for {
      members <- Stateful.get[F, State].map(_.roomMembers.get(room))
      _ <- members match {
        case Some(members) => Tell[F, Seq[OutputMessage]].tell(Seq(SendToUsers(members, text)))
        case None          => Monad[F].unit
      }
    } yield ()

    override def addToRoom(room: Room, user: User): F[Unit] = for {
      members <- Stateful.get[F, State].map(_.roomMembers.getOrElse(room, Set()) + user)
      _ <- Stateful.modify[F, State] { state =>
        State(
          state.userRooms + (user   -> room),
          state.roomMembers + (room -> members),
          state.player + (user      -> DefaultPlayer)
        )
      } *> sendToRoom(room, s"$user has joined $room")
    } yield ()

    override def roomSize(room: Room): F[Int] =
      Stateful.get[F, State].map(_.roomMembers.get(room).map(_.size).getOrElse(0))
  }
}
