package game

import game.model.Player.{CombinationsDice, Player}
import server.model._

object Room {
  val DefaultCombinationsDice: CombinationsDice = CombinationsDice.of()
  val DefaultPlayer: Player = Player.of(List(DefaultCombinationsDice))

  def removeFromCurrentRoom(
      user: String
  )(
      state: State
  ): (State, Seq[OutputMessage]) = state.userRooms.get(user) match {
    case Some(room) =>
      val nextMembers = state.roomMembers.getOrElse(room, Set()) - user
      val nextState =
        if (nextMembers.isEmpty)
          State(
            state.userRooms - user,
            state.roomMembers - room,
            state.player - user
          )
        else
          State(
            state.userRooms - user,
            state.roomMembers + (room -> nextMembers),
            state.player - user
          )

      (
        nextState,
        sendToRoom(room, s"$user has left $room")(nextState.roomMembers)
      )
    case None =>
      (state, Nil)
  }

  def sendToRoom(room: String, text: String)(
      roomMembers: Map[String, Set[String]]
  ): Seq[OutputMessage] = {
    roomMembers
      .get(room)
      .map(SendToUsers(_, text))
      .toSeq
  }

  def addToRoom(
      user: String,
      room: String
  )(
      state: State
  ): (State, Seq[OutputMessage]) = {
    val nextMembers = state.roomMembers.getOrElse(room, Set()) + user
    val nextState = State(
      state.userRooms + (user -> room),
      state.roomMembers + (room -> nextMembers),
      state.player + (user -> DefaultPlayer)
    )

    (
      nextState,
      sendToRoom(room, s"$user has joined $room")(nextState.roomMembers)
    )
  }

  def sizeRoom(toRoom: String)(
      roomMembers: Map[String, Set[String]]
  ): Int = roomMembers
    .getOrElse(toRoom, Set())
    .toList
    .size
}
