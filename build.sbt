lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.12.5",
      version      := "0.1.0"
    )),
    name := "Deflate",
    libraryDependencies ++= Seq(
        "org.scalactic" %% "scalactic" % "3.0.4",
        "org.scalatest" %% "scalatest" % "3.0.4" % "test"
        )
  )
