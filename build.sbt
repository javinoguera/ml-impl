name := "ml"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "com.github.vinhkhuc" % "lbfgs4j" % "0.2.1"

// Repositories
resolvers += "Maven Central Server" at "http://repo1.maven.org/maven2"

