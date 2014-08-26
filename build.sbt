name := "NFL"

version:= "1.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
	"com.typesafe.akka" % "akka" % "2.0.2",
	"com.typesafe.akka" % "akka-actor" % "2.0.2",
	"org.specs2" %% "specs2" % "1.12.4.1" % "test")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)
