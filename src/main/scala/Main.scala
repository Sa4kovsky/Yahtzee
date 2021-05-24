import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import fs2.concurrent.{Queue, Topic}
import game.GameState
import server.Server
import server.model._

import scala.concurrent.duration.DurationInt

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for (
      queue <- Queue.unbounded[IO, InputMessage];
      topic <- Topic[IO, OutputMessage](SendToUsers(Set.empty, ""));
      ref   <- Ref.of[IO, GameState](GameState());

      exitCode <- {
        val httpStream = Server.stream[IO](ref, queue, topic)

        val keepAlive = Stream
          .awakeEvery[IO](120.seconds)
          .map(_ => KeepAlive)
          .through(topic.publish)

        val processingStream =
          queue.dequeue
            .evalMap { msg => ref.modifyState(GameState.process(msg)) }
            .flatMap(Stream.emits)
            .through(topic.publish)

        Stream(
          httpStream,
          keepAlive,
          processingStream
        ).parJoinUnbounded.compile.drain
          .as(ExitCode.Success)
      }
    ) yield exitCode
  }
}
