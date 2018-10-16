organization := "org.goldenport"

name := "goldenport-record"

version := "2.0.5"

scalaVersion := "2.12.7"

// crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.goldenport" %% "goldenport-atom" % "2.0.0"

libraryDependencies += "org.smartdox" %% "smartdox" % "2.0.2-SNAPSHOT"

// TODO remove
libraryDependencies += "org.scalikejdbc" %% "scalikejdbc" % "3.3.1" % "provided"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

// TODO remove : V1 Record dependency
libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
