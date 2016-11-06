lazy val buildSettings = Seq(
  version := "0.2.0-SNAPSHOT",
  organization := "com.eed3si9n",
  homepage := Some(url("http://eed3si9n.com")),
  description := "This program was copied from `eed3si9n` as my hobby and study",
  licenses := Seq("MIT License" -> url("http://opensource.org/licenses/mit-license.php/")),
  scalaVersion in ThisBuild := "2.11.8",
  scalacOptions := Seq("-deprecation", "-unchecked"),
  initialCommands in console := "",

  resolvers += Resolver.sonatypeRepo("public")
)

lazy val specs2version = "3.8.5"
lazy val libDeps = Def.setting {
  Seq(
    "org.specs2" %% "specs2-core" % specs2version % "test",
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
  )
}

lazy val swingDepencencies = Def.setting {
  "org.scala-lang" % "scala-swing" % "2.11.0-M7"
}

lazy val root = (project in file(".")).
  settings(buildSettings: _*).
  settings(name := "tetrix.scala")

lazy val library = (project in file("library")).
  settings(buildSettings: _*).
  settings(
    libraryDependencies ++= libDeps.value
  )

lazy val swing = (project in file("swing")).
  settings(buildSettings: _*).
  settings(
    fork in run := true,
    libraryDependencies += swingDepencencies.value
  ).
  dependsOn(library)
