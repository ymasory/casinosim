//BASIC PROJECT INFO
name := "casinosim"

organization := "com.yuvimasory"

version := "0.1.0-SNAPSHOT"

//SCALA VERSIONS AND OPTIONS
scalaVersion := "2.10.0"

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  "-unchecked"
)

javacOptions ++= Seq(
  "-Xlint:unchecked",
  "-Xlint:deprecation"
)

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.0"

//ENTRY POINT
mainClass in (Compile, packageBin) := Some("com.yuvimasory.casinosim.Main")

mainClass in (Compile, run) := Some("com.yuvimasory.casinosim.Main")

//SBT BEHAVIOR
fork in Test := true

fork in Compile := true

logLevel := Level.Info

traceLevel := 5

