import sbt._

object Dependencies {

  object Cats {
    private val catsVersion = "2.2.0"
    val core = "org.typelevel" %% "cats-core" % catsVersion
    val effects = "org.typelevel" %% "cats-effect" % catsVersion

    val all = Seq(core, effects)
  }

  object UnitTest {
    val scalaTest = "org.scalatest" %% "scalatest" % "3.2.3" % Test
    val scalaMock = "org.scalamock" %% "scalamock" % "4.4.0" % Test
    val catsHelperTestKil =
      "com.evolutiongaming" %% "cats-helper-testkit" % "2.1.3"

    val all = Seq(scalaTest, scalaMock, catsHelperTestKil)
  }

  object Http4s {
    private val http4sVersion = "0.21.22"
    val dsl = "org.http4s" %% "http4s-dsl" % http4sVersion
    val server = "org.http4s" %% "http4s-blaze-server" % http4sVersion
    val client = "org.http4s" %% "http4s-blaze-client" % http4sVersion
    val blazeClient = "org.http4s" %% "http4s-blaze-client" % http4sVersion
    val circle = "org.http4s" %% "http4s-circe" % http4sVersion

    val all = Seq(dsl, server, client, blazeClient, circle)
  }

}
