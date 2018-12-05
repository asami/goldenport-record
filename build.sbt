organization := "org.goldenport"

name := "goldenport-record"

version := "1.2.26"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

// libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

libraryDependencies += "org.goldenport" %% "goldenport-atom" % "1.0.1"

// libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.2"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.2.28"

libraryDependencies += "org.smartdox" %% "smartdox" % "1.2.2"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "provided"

libraryDependencies += "net.sf.opencsv" % "opencsv" % "2.3" % "provided"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.4.1" % "provided"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

// libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "1.0.1" % "test"

libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
