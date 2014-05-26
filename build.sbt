name := "graph"

scalaVersion := "2.10.3"

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-language:implicitConversions"
)

val scalazVersion = "7.0.6"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion
)
