import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, Sync}
import fs2.concurrent.{Queue, Topic}
import fs2.{Pipe, Stream}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.{Close, Text}

object GameRoutes {

  def routes[F[_]: Sync: ContextShift](
      chatState: Ref[F, RoomState],
      queue: Queue[F, InputMessage],
      topic: Topic[F, OutputMessage]
  ) = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    // ws://127.0.0.1:8080/game/TestName
    HttpRoutes.of[F] { case GET -> Root / "game" / userName =>
      val toClient: Stream[F, WebSocketFrame.Text] =
        topic
          .subscribe(1000)
          .filter(_.forUser(userName))
          .map(msg => Text(msg.toString))

      def processInput(
          wsfStream: Stream[F, WebSocketFrame]
      ): Stream[F, Unit] = {
        val entryStream: Stream[F, InputMessage] =
          Stream.emits(Seq(EnterRoom(userName, InputMessage.DefaultRoomName)))

        val parsedWebSocketInput: Stream[F, InputMessage] =
          wsfStream
            .collect {
              case Text(text, _) => InputMessage.parse(userName, text)
              case Close(_)      => Disconnect(userName)
            }

        (entryStream ++ parsedWebSocketInput).through(queue.enqueue)
      }

      val inputPipe: Pipe[F, WebSocketFrame, Unit] = processInput

      WebSocketBuilder[F].build(toClient, inputPipe)
    }
  }
}