package game

import game.model.Player.Player
import game.model.{Room, User}
import server.model._

object Room {
  def removeFromCurrentRoom(user: User)(implicit state: GameState): (GameState, Seq[OutputMessage]) =
    state.userRooms.get(user) match {
      case Some(room) =>
        val nextMembers = state.roomMembers.getOrElse(room, Set()) - user
        val nextState =
          if (nextMembers.isEmpty)
            GameState(state.userRooms - user, state.roomMembers - room, state.player - user)
          else
            GameState(state.userRooms - user, state.roomMembers + (room -> nextMembers), state.player - user)

        (
          nextState,
          Seq(
            SendToUsers(nextState.roomMembers.getOrElse(room, Set.empty), s"$user has left $room")
          )
        )
      case None => (state, Nil)
    }

  def addToRoom(user: User, room: Room)(implicit state: GameState): (GameState, Seq[OutputMessage]) = {
    val nextMembers = state.roomMembers.getOrElse(room, Set()) + user
    val nextState = GameState(
      state.userRooms + (user   -> room),
      state.roomMembers + (room -> nextMembers),
      state.player + (user      -> Player.Default)
    )

    (nextState, Seq(SendToUsers(nextState.roomMembers.getOrElse(room, Set.empty), s"$user has joined $room")))

  }

  def sizeRoom(toRoom: Room)(implicit state: GameState): Int =
    state.roomMembers
      .getOrElse(toRoom, Set())
      .toList
      .size
}
