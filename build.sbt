import sbt._

lazy val versions = new {
  val scalacheck = "1.13.4"
  val scalatest = "3.0.3"
  val scalaz = "7.2.17"
}

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "org.refeed",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT",
      scalacOptions ++= Seq(
        "-deprecation",
        "-unchecked",
        "-explaintypes",
        "-feature",
        "-unchecked",
        "-Ywarn-dead-code",
        "-Ypartial-unification",
        "-Ywarn-unused:imports",
        "-Ywarn-unused:locals",
        "-Xfatal-warnings"
      ),
      scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
      resolvers ++= Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots")
      )
    )),
  name := "advent-of-code",
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "io.spray" %%  "spray-json" % "1.3.2",
    "org.scalaz" %% "scalaz-core" % versions.scalaz,
    "org.scalacheck" %% "scalacheck" % versions.scalacheck % "test",
    "org.scalatest" %% "scalatest" % versions.scalatest % "test"
  )
)
