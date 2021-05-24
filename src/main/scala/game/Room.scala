package game

import game.model.Player.Player
import game.model.{Room, User}
import server.model._

object Room {
  def removeFromCurrentRoom(user: User)(state: State): (State, Seq[OutputMessage]) =
    state.userRooms.get(user) match {
      case Some(room) =>
        val nextMembers = state.roomMembers.getOrElse(room, Set()) - user
        val nextState =
          if (nextMembers.isEmpty)
            State(state.userRooms - user, state.roomMembers - room, state.player - user)
          else
            State(state.userRooms - user, state.roomMembers + (room -> nextMembers), state.player - user)

        (nextState, sendToRoom(room, s"$user has left $room")(nextState.roomMembers))

      case None => (state, Nil)
    }

  def sendToRoom(room: Room, text: String)(roomMembers: Map[Room, Set[User]]): Seq[SendToUsers] = {
    roomMembers
      .get(room)
      .map(SendToUsers(_, text))
      .toSeq
  }

  def addToRoom(user: User, room: Room)(state: State): (State, Seq[OutputMessage]) = {
    val nextMembers = state.roomMembers.getOrElse(room, Set()) + user
    val nextState = State(
      state.userRooms + (user   -> room),
      state.roomMembers + (room -> nextMembers),
      state.player + (user      -> Player.Default)
    )

    (nextState, sendToRoom(room, s"$user has joined $room")(nextState.roomMembers))
  }

  def sizeRoom(toRoom: Room)(roomMembers: Map[Room, Set[User]]): Int =
    roomMembers
      .getOrElse(toRoom, Set())
      .toList
      .size
}
