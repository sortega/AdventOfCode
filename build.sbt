name := "advent-of-code"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "io.spray" %%  "spray-json" % "1.3.2",
  "org.scalaz" %% "scalaz-core" % "7.1.5",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)
