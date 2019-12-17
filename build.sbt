organization := "org.goldenport"

name := "goldenport-record"

version := "2.1.3-SNAPSHOT"

scalaVersion := "2.12.7"

// crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.goldenport" %% "goldenport-atom" % "2.1.0"

libraryDependencies += "org.smartdox" %% "smartdox" % "2.1.1"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.1.2"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.10" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.1.1"

libraryDependencies += "net.sf.opencsv" % "opencsv" % "2.3" % "compile"

libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.1" % "compile"

libraryDependencies += "com.zaxxer" % "HikariCP-java7" % "2.4.13" % "compile"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

// TODO remove : V1 Record dependency
libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
