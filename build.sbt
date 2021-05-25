import sbt.compilerPlugin

name := "Yahtzee"

version := "0.1"

scalaVersion := "2.13.5"

val http4sVersion      = "0.21.22"
val circeVersion       = "0.13.0"
val catsVersion        = "2.2.0"
val catsTaglessVersion = "0.11"
val catsEffectVersion  = "2.2.0"
val Specs2Version      = "4.8.0"
val scalaTestVersion   = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.typelevel"     %% "cats-core"                % catsVersion,
  "org.typelevel"     %% "cats-mtl"                 % "1.2.0",
  "org.typelevel"     %% "cats-effect"              % catsEffectVersion,
  "org.http4s"        %% "http4s-dsl"               % http4sVersion,
  "org.http4s"        %% "http4s-blaze-server"      % http4sVersion,
  "org.http4s"        %% "http4s-blaze-client"      % http4sVersion,
  "org.http4s"        %% "http4s-circe"             % http4sVersion,
  "org.http4s"        %% "http4s-jdk-http-client"   % "0.3.6",
  "ch.qos.logback"    % "logback-classic"           % "1.2.3",
  "org.slf4j"         % "slf4j-nop"                 % "1.6.4",
  "io.circe"          %% "circe-core"               % circeVersion,
  "io.circe"          %% "circe-generic"            % circeVersion,
  "io.circe"          %% "circe-generic-extras"     % circeVersion,
  "io.circe"          %% "circe-optics"             % circeVersion,
  "io.circe"          %% "circe-parser"             % circeVersion,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test,
  "org.scalatestplus" %% "selenium-2-45"            % scalaTestVersion % Test,
  "org.specs2"        %% "specs2-core"              % Specs2Version % "test",
  compilerPlugin("org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary)
)

scalafmtOnCompile := true
