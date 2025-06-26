ThisBuild / scalaVersion     := "3.3.1"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.lambdamu"

lazy val root = (project in file("."))
  .settings(
    name := "lambda-mu-calculus",
    scalacOptions ++= Seq(
      "-deprecation",            // Warn about deprecated APIs
      "-feature",                // Warn about features that should be explicitly imported
      "-unchecked",              // Enable additional warnings for unchecked type patterns
      "-Xfatal-warnings",        // Turn warnings into errors
      "-explain",                // Explain type errors in detail
      "-source:future"           // Enable forward-looking Scala features
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    )
  )

