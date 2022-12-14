
lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.10",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
