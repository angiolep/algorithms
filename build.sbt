
lazy val root = (project in (file("."))).
  settings(
    organization := "bitspoke",
    name := "algorithms",
    version := "1.0",
    scalaVersion := "2.11.7"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0-M7" % "test"
)  


