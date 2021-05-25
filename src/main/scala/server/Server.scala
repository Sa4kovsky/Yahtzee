package server

import cats.effect.concurrent.Ref
import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, Timer}
import fs2.Stream
import fs2.concurrent.{Queue, Topic}
import game.GameState
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import server.model._

object Server {
  def stream[F[_]: ConcurrentEffect: Timer: ContextShift](
    gameState: Ref[F, GameState],
    queue: Queue[F, InputMessage],
    topic: Topic[F, OutputMessage]
  ): Stream[F, ExitCode] =
    BlazeServerBuilder[F]
      .bindHttp(8080)
      .withHttpApp(
        Router(
          "/" -> Routes.routes[F](gameState, queue, topic)
        ).orNotFound
      )
      .serve
}
