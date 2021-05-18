import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import fs2.concurrent.{Queue, Topic}

import scala.concurrent.duration.DurationInt

case class State(messageCount: Int)

object Main extends IOApp {
  def run(args: List[String]) = {
    for (
      queue <- Queue.unbounded[IO, InputMessage];
      topic <- Topic[IO, OutputMessage](SendToUsers(Set.empty, ""));
      ref <- Ref.of[IO, RoomState](RoomState());

      exitCode <- {
        val httpStream = GameServer.stream[IO](ref, queue, topic)

        val keepAlive = Stream
          .awakeEvery[IO](120.seconds)
          .map(_ => KeepAlive)
          .through(topic.publish)

        val processingStream =
          queue.dequeue
            .evalMap(msg => ref.modify(_.process(msg)))
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
