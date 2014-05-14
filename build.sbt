organization := "org.goldenport"

name := "goldenport-record"

version := "1.0.2"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.5"

libraryDependencies += "org.goldenport" %% "goldenport-atom" % "1.0.0"

// libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.2"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.0.0"

libraryDependencies += "org.smartdox" %% "smartdox" % "1.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "1.0.0" % "test"

libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "0.8.0"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
