organization := "org.goldenport"

name := "goldenport-record"

version := "2.0.5"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.goldenport" %% "goldenport-atom" % "2.0.0"

libraryDependencies += "org.smartdox" %% "smartdox" % "2.0.1"

libraryDependencies += "org.scalikejdbc" %% "scalikejdbc" % "2.2.6" % "provided"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.3" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

// TODO remove : V1 Record dependency
libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
