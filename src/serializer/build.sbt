name := "jaam-serializer"
version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.ucombinator.soot" % "soot" % "nightly.20161021",
  "com.twitter" %% "chill" % "0.8.0",
  "de.javakaffee" % "kryo-serializers" % "0.38"
)
