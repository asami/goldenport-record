organization := "org.goldenport"

name := "goldenport-record-http"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "1.2.21-SNAPSHOT"

// libraryDependencies += "com.softwaremill.sttp" %% "core" % "1.3.0" // 2.11

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.4.1"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

// libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "1.0.1" % "test"

libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
