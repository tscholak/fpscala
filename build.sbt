name := "fpscala"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.11.8",
  "org.scala-lang" % "scala-reflect" % "2.11.8",
  "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5",
  "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
  "com.typesafe.akka" % "akka-actor_2.11" % "2.4.2",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.1"
)

//coverageMinimum := 80
//coverageFailOnMinimum := false
