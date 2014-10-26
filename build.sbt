name := "NFL"

version:= "1.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
	"org.specs2" %% "specs2" % "1.12.4.1" % "test",
	"com.typesafe.akka" % "akka-actor"        % "2.0.3" withSources,
  	"com.typesafe.akka" % "akka-testkit"      % "2.0.3" % "test" withSources)

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)
