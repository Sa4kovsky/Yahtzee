import cats.effect.concurrent.Ref
import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, Timer}
import fs2.Stream
import fs2.concurrent.{Queue, Topic}
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

object GameServer {
  def stream[F[_]: ConcurrentEffect: Timer: ContextShift](
      chatState: Ref[F, RoomState],
      queue: Queue[F, InputMessage],
      topic: Topic[F, OutputMessage]
  ): Stream[F, ExitCode] =
    BlazeServerBuilder[F]
      .bindHttp(8080)
      .withHttpApp(
        Router(
          "/" -> GameRoutes.routes[F](chatState, queue, topic)
        ).orNotFound
      )
      .serve
}
