//BASIC PROJECT INFO
name := "casinosim"

organization := "com.yuvimasory"

version := "0.1.0-SNAPSHOT"

//SCALA VERSIONS AND OPTIONS
scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation", "-unchecked")

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

//ENTRY POINT
mainClass in (Compile, packageBin) := Some("com.yuvimasory.casinosim.Main")

mainClass in (Compile, run) := Some("com.yuvimasory.casinosim.Main")

//SBT BEHAVIOR
fork in Test := true

fork in Compile := true

logLevel := Level.Info

traceLevel := 5

