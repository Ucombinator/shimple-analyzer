// TODO: error if sbt called directly on a sub-project

organization in ThisBuild := "org.ucombinator"
scalaVersion in ThisBuild := "2.11.8"

resolvers += "Ucombinator maven repository on github" at "https://ucombinator.github.io/maven-repo"
resolvers += Resolver.sonatypeRepo("public")

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

  // Use shading to avoid file conflicts in some problematic dependencies
  assemblyShadeRules in assembly := Seq(
    ShadeRule.rename("org.objectweb.**" -> "shadedSoot.@0")
      .inProject,
    ShadeRule.rename("org.objectweb.**" -> "shadedSoot.@0")
      .inLibrary("org.ucombinator.soot" % "soot-all-in-one" % "nightly.20150205"),
    ShadeRule.rename("com.esotericsoftware.**" -> "shaded-kryo.@0")
      .inLibrary("com.esotericsoftware" % "kryo-shaded" % "3.0.3")
  )
)
