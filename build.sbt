// TODO: error if sbt called directly on a sub-project

organization in ThisBuild := "org.ucombinator"
scalaVersion in ThisBuild := "2.11.8"

// Create assemblies only if we explicitly ask for them
disablePlugins(sbtassembly.AssemblyPlugin)

////////////////////////////////////////
// Sub-projects
////////////////////////////////////////

lazy val serializer = (project in file("src/serializer")).settings(commonSettings)

lazy val tools = (project in file("src/tools")).settings(commonSettings).dependsOn(serializer)

lazy val interpreter = (project in file("src/interpreter")).settings(commonSettings).dependsOn(serializer)

lazy val visualizer = (project in file("src/visualizer")).settings(commonSettings).dependsOn(serializer)

lazy val analyzer = (project in file("src/analyzer")).settings(commonSettings).dependsOn(serializer)

lazy val json_exporter = (project in file("src/json_exporter")).settings(commonSettings).dependsOn(serializer)

////////////////////////////////////////
// Global settings
////////////////////////////////////////

// A "discard" merge strategy that doesn't cause a warning
lazy val quietDiscard = new sbtassembly.MergeStrategy {
  val name = "quietDiscard"
  def apply(tempDir: File, path: String, files: Seq[File]): Either[String, Seq[(File, String)]] =
    Right(Nil)
  override def detailLogLevel = Level.Debug
  override def summaryLogLevel = Level.Info
  override def notifyThreshold = 1
}

// Settings shared between sub-projects
lazy val commonSettings = Seq(
  // Use repository containing soot-all-in-one nightly snapshot
  resolvers += "Ucombinator maven repository on github" at "https://ucombinator.github.io/maven-repo",

  // Flags to 'scalac'.  Try to get as much error and warn detection as possible.
  scalacOptions ++= Seq(
    // Emit warning and location for usages of deprecated APIs.
    "-deprecation",
    // Explain type errors in more detail.
    "–explaintypes",
    // Emit warning and location for usages of features that should be imported explicitly.
    "-feature",
    // Generates faster bytecode by applying optimisations to the program
    "-optimise",
    // Enable additional warnings where generated code depends on assumptions.
    "-unchecked",
    "-Xlint:_"
  ),

  // Discard META-INF, but deduplicate everything else
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => quietDiscard
    case x => MergeStrategy.deduplicate
  },

  // https://mvnrepository.com/artifact/org.ow2.asm/asm-tree
  libraryDependencies += "org.ow2.asm" % "asm-tree" % "5.1",

  // https://mvnrepository.com/artifact/org.ow2.asm/asm-commons
  libraryDependencies += "org.ow2.asm" % "asm-commons" % "5.1",

  // https://mvnrepository.com/artifact/com.google.guava/guava
  libraryDependencies += "com.google.guava" % "guava" % "20.0",

  libraryDependencies += "org.ucombinator.heros" % "heros" % "nightly.20161021",

  // https://mvnrepository.com/artifact/org.slf4j/slf4j-api
  libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.21",

  // Use shading to avoid file conflicts in some problematic dependencies
  assemblyShadeRules in assembly := Seq(
    ShadeRule.rename("com.esotericsoftware.**" -> "shaded-kryo.@0")
      .inLibrary("com.esotericsoftware" % "kryo-shaded" % "3.0.3")
  )
)
