name := "fpscala"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "org.scalaz" %% "scalaz-core" % "7.2.2",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.2",
  "com.typesafe.akka" %% "akka-actor" % "2.4.4"
)

//coverageMinimum := 80
//coverageFailOnMinimum := false
