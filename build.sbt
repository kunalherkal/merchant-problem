name := """merchant-problem"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
 "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.0-RC1" % Test,
  "org.scalatest" % "scalatest_2.11" % "2.2.6" % Test
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6"

