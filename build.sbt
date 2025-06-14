name := "LambdaMuScala3"

version := "0.1.0"

// *** Use a literal String here, not ThisBuild / scalaVersion ***
scalaVersion := "3.3.1"

// Parser-combinators for your JavaTokenParsers
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

// Tell sbt which main to run
Compile / mainClass := Some("lambdamu.runRepl")

// Fork a new JVM so that StdIn works correctly
Compile / run / fork := false

// (Optional) compiler flags
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked"
)

