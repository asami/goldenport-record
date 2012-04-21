organization := "org.goldenport"

name := "goldenport-record"

version := "0.1.1"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.3"

libraryDependencies += "org.goldenport" %% "goldenport-atom" % "0.1.0"

libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.0"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "0.1.2"

libraryDependencies += "org.smartdox" %% "smartdox" % "0.2.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

libraryDependencies += "org.goldenport" %% "scalatestlib" % "0.1.0" % "test"

libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
