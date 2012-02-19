//BASIC PROJECT INFO
name := "casinosim"

organization := "com.yuvimasory"

version := "alpha"

//SCALA VERSIONS AND OPTIONS
scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

//ENTRY POINT
mainClass in (Compile, packageBin) := Some("com.yuvimasory.casinosim.Main")

mainClass in (Compile, run) := Some("com.yuvimasory.casinosim.Main")

//SCALA DEPENDENCIES
//only uncomment if you need dependencies from the snapshots repo
//resolvers += ScalaToolsSnapshots

//JAVA DEPENDENCIES
libraryDependencies ++= Seq (
  //"com.martiansoftware" % "jsap" % "2.1"
)

//SBT BEHAVIOR
fork in Test := true

fork in Compile := true

logLevel := Level.Info //higher than Info suppresses your own printlns

traceLevel := 5

//PROGUARD
seq(ProguardPlugin.proguardSettings :_*)

proguardOptions ++= Seq (
    "-dontshrink -dontoptimize -dontobfuscate -dontpreverify -dontnote " +
    "-ignorewarnings",
    keepAllScala
)


//PUBLISHING

//this results in warnings if the listed file doesn't exist
// credentials += Credentials(Path.userHome / ".scala-tools")

publishMavenStyle := true

publishArtifact in (Test, packageBin) := false

publishArtifact in (Compile, packageDoc):= false

publishArtifact in (Compile, packageSrc):= false


