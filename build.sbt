val commonSettings = Seq(
  scalaVersion := "2.13.6",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(
    name := "answers"
  )
